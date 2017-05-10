open Sql

type t =
  {
    db : db;
    procs : int array;
    mutable caches : (string * string) list;
  }

type proc_id = int

(* Low-level functions *)

let tables db =
  results
    db
    Text
    "SELECT name FROM sqlite_master WHERE type = 'table';"

(* Caching *)

let cached_activities =
  [
    "GC", "GC_ENTER", "GC_LEAVE";
    "Runtime", "RUNTIME_ENTER", "RUNTIME_LEAVE";
    "LockTaking", "LOCK_TAKE_ENTER", "LOCK_TAKE_LEAVE";
    "LockHolding", "LOCK_TAKE_LEAVE", "LOCK_RELEASE";
    "GSection", "GSECTION_BEGIN_ENTER", "GSECTION_END_LEAVE";
    "GSectionWork", "GSECTION_BEGIN_LEAVE", "GSECTION_END_ENTER";
    "GSectionEntering", "GSECTION_BEGIN_ENTER", "GSECTION_BEGIN_LEAVE";
    "GSectionLeaving", "GSECTION_END_ENTER", "GSECTION_END_LEAVE";
  ]

let purge trace =
  let keep = [ "events" ] in
  let drop s =
    exec_check
      trace.db
      (Printf.sprintf "DROP TABLE %s;" s)
  in
  List.(tables trace.db
        |> filter (fun s -> not @@ List.mem s keep)
        |> iter drop);
  trace.caches <- [];
  ()

let cache_table_name name =
  name ^ "_activities"

let create_activity_cache trace ~name ~enter ~leave =
  (* Create the activity table *)
  let table = cache_table_name name in
  let create_req =
    Printf.sprintf
      "CREATE TEMPORARY TABLE %s(
         argptr INTEGER,
         enter REAL,
         leave REAL,
         PRIMARY KEY(argptr, enter)
       );"
      table
  in
  exec_check trace.db create_req;
  (* Create two temporary tables for labeling. *)
  let create_temp ~name ~kind =
    let req =
      Printf.sprintf
        "CREATE TABLE %s(
           id INTEGER,
           argptr INTEGER,
           time REAL,
           PRIMARY KEY(id)
         );
         INSERT INTO %s
         SELECT NULL, argptr, time
         FROM events
         WHERE kind = '%s'
         ORDER BY argptr, time;"
        name
        name
        kind
    in
    exec_check trace.db req
  in
  create_temp ~name:"tmp1" ~kind:enter;
  create_temp ~name:"tmp2" ~kind:leave;
  (* Merge events with matching ids *)
  let merge_req =
    Printf.sprintf
      "INSERT INTO %s
       SELECT e.argptr, e.time, l.time
       FROM tmp1 e, tmp2 l
       WHERE e.id = l.id;"
      table
  in
  exec_check trace.db merge_req;
  exec_check trace.db "DROP TABLE tmp1; DROP TABLE tmp2;";
  trace.caches <- (name, table) :: trace.caches;
  table

let find_activity_cache trace ~name ~enter ~leave =
  try List.assoc name trace.caches
  with Not_found -> create_activity_cache trace ~name ~enter ~leave

let prepare ?(verbose = false) trace =
  let create_cache (name, enter, leave) =
    if List.mem_assoc name trace.caches
    then
      begin
        if verbose
        then Format.eprintf "Cache for %s already present, skipping@." name
      end
    else
      begin
        if verbose then Format.eprintf "Creating cache table for %s@." name;
        let start = Unix.gettimeofday () in
        ignore @@ create_activity_cache trace ~name ~enter ~leave;
        let stop = Unix.gettimeofday () in
        if verbose then Format.eprintf "Done in %.2f seconds@." (stop -. start);
      end
  in
  List.iter create_cache cached_activities;
  ()

let processor_ids db =
  results
    db
    Int
    "SELECT DISTINCT(argptr) FROM events;"
  |> Array.of_list

(* Exposed functions *)

let from_sqlite_file filename =
  let db = db_open ~mode:`NO_CREATE filename in
  let tables = tables db in

  (* Check that the event table is here *)
  if not @@ List.mem "events" tables
  then failwith (filename ^ ": missing event table");

  (* Check that we find some events *)
  let procs = processor_ids db in
  if Array.length procs = 0
  then failwith (filename ^ ": no events?");

  (* Populate the cache *)
  let caches =
    let add caches (name, _, _) =
      let table = cache_table_name name in
      if List.mem table tables then (name, table) :: caches else caches
    in
    List.fold_left add [] cached_activities
  in

  {
    db;
    procs;
    caches;
  }

let epoch { db; _ } =
  let l, u =
    result
      db
      (Pair (Real, Real))
      "SELECT MIN(time), MAX(time) FROM events;"
  in
  Range.{ l; u; }

let number_of_processors { procs; _ } =
  Array.length procs

let activities_between
      trace
      ~name
      ~enter
      ~leave
      ~between
      ~min_duration
      ~proc
      () =
  let table = find_activity_cache trace ~name ~enter ~leave in
  let req =
    Printf.sprintf
      "SELECT enter, leave
       FROM %s
       WHERE argptr = %d AND leave - enter >= %f
       AND ((%f <= enter AND leave <= %f)
            OR (%f <= leave AND enter <= %f)
            OR (enter <= %f AND leave >= %f));"
      table
      (trace.procs.(proc))
      min_duration
      between.Range.l
      between.Range.u
      between.Range.l
      between.Range.u
      between.Range.l
      between.Range.u
  in
  let res = results trace.db (Pair (Real, Real)) req in
  List.map (fun (l, u) -> Range.{ l; u; }) res

let events_between { db; procs; } ~between ~proc ~kind () =
  let req =
    Printf.sprintf
      "SELECT time
       FROM events
       WHERE kind = \"%s\" AND %f <= time AND time <= %f AND argptr = %d;"
      kind
      between.Range.l
      between.Range.u
      procs.(proc)
  in
  results db Real req

let max_occupancy { db; _ } =
  try
    let req = "SELECT max(arg1) FROM events WHERE kind = \"HEAP_OCCUPANCY\";" in
    result db Real req
  with _ ->
    0.

let occupancy_between { db; _ } ~between ~granularity () =
  (* granularity ignored for now *)
  let req =
    Printf.sprintf
      "SELECT time, arg2 FROM events
       WHERE kind = \"HEAP_OCCUPANCY\" AND %f <= time AND time <= %f
       ORDER BY time;"
      between.Range.l
      between.Range.u
  in
  results db (Pair (Real, Real)) req

let max_ratio { db; procs } ~proc =
  try
    let req =
      Printf.sprintf
        "SELECT max(arg1/arg2)
         FROM events
         WHERE kind = \"HEAP_RATIO\" AND argptr = %d AND arg2 > 0;"
        procs.(proc)
    in
    result db Real req
  with _ ->
    0.

let ratio_between { db; procs } ~between ~proc ~granularity () =
  (* granularity ignored for now *)
  let req =
    Printf.sprintf
      "SELECT time, (arg1/arg2) FROM events
       WHERE kind = \"HEAP_RATIO\" AND %f <= time AND time <= %f
       AND argptr = %d AND arg2 > 0
       ORDER BY time;
       "
      between.Range.l
      between.Range.u
      procs.(proc)
  in
  results db (Pair (Real, Real)) req

let max_locally_collectible { db; procs; } ~proc () =
  try
    let req =
      Printf.sprintf
        "SELECT max(arg2)
         FROM events
         WHERE kind = \"HEAP_RATIO\" AND argptr = %d;"
        procs.(proc)
    in
    result db Real req
  with _ ->
    0.

let locally_collectible_between { db; procs } ~between ~proc ~granularity () =
  (* granularity ignored for now *)
  let req =
    Printf.sprintf
      "SELECT time, arg2 FROM events
       WHERE kind = \"HEAP_RATIO\" AND %f <= time AND time <= %f
       AND argptr = %d
       ORDER BY time;
       "
      between.Range.l
      between.Range.u
      procs.(proc)
  in
  results db (Pair (Real, Real)) req

let max_locally_collectible_heap { db; procs; } ~proc () =
  try
    let req =
      Printf.sprintf
        "SELECT max(arg1)
         FROM events
         WHERE kind = \"HEAP_RATIO\" AND argptr = %d;"
        procs.(proc)
    in
    result db Real req
  with _ ->
    0.

let locally_collectible_heap_between
      { db; procs }
      ~between
      ~proc
      ~granularity
      () =
  (* granularity ignored for now *)
  let req =
    Printf.sprintf
      "SELECT time, arg1 FROM events
       WHERE kind = \"HEAP_RATIO\" AND %f <= time AND time <= %f
       AND argptr = %d
       ORDER BY time;
       "
      between.Range.l
      between.Range.u
      procs.(proc)
  in
  results db (Pair (Real, Real)) req

let max_control_ratio { db; procs; } ~proc () =
  (* granularity ignored for now *)
  let req =
    Printf.sprintf
      "SELECT max(arg3) FROM events
       WHERE kind = \"HEAP_RATIO\" AND argptr = %d
       ORDER BY time;
       "
      procs.(proc)
  in
  try result db Real req with _ -> 0.
