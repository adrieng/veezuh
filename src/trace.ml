open Sql

type t =
  {
    db : Sql.t;
    procs : int array;
    mutable caches : (string * string) list;
  }

type proc_id = int

(* Low-level functions *)

let tables db =
  results
    db
    text
    "SELECT name FROM sqlite_master WHERE type = 'table';"

(* Caching *)

let gc_events =
  "GC", "GC_ENTER", "GC_LEAVE"

let gsection_events =
  "GSection", "GSECTION_BEGIN_ENTER", "GSECTION_END_LEAVE"

let cached_activities =
  [
    gc_events;
    "Runtime", "RUNTIME_ENTER", "RUNTIME_LEAVE";
    "LockTaking", "LOCK_TAKE_ENTER", "LOCK_TAKE_LEAVE";
    gsection_events;
    "GSectionWork", "GSECTION_BEGIN_LEAVE", "GSECTION_END_ENTER";
    "GSectionEntering", "GSECTION_BEGIN_ENTER", "GSECTION_BEGIN_LEAVE";
    "GSectionLeaving", "GSECTION_END_ENTER", "GSECTION_END_LEAVE";
    "ArrayAllocate", "ARRAY_ALLOCATE_ENTER", "ARRAY_ALLOCATE_LEAVE";
  ]

let purge trace =
  let keep = [ "events" ] in
  let drop s =
    execute
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
      "CREATE TABLE %s(
         argptr INTEGER,
         enter REAL,
         leave REAL,
         PRIMARY KEY(argptr, enter)
       );"
      table
  in
  execute trace.db create_req;
  (* Create two temporary tables for labeling. *)
  let create_temp ~name ~kind =
    let req =
      Printf.sprintf
        "CREATE TEMPORARY TABLE %s(
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
    execute trace.db req
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
  execute trace.db merge_req;
  execute trace.db "DROP TABLE tmp1; DROP TABLE tmp2;";
  trace.caches <- (name, table) :: trace.caches;
  table

let find_activity_cache trace ~name ~enter ~leave =
  try List.assoc name trace.caches
  with Not_found -> create_activity_cache trace ~name ~enter ~leave

let find_gc_cache_table trace =
  let name, enter, leave = gc_events in
  find_activity_cache trace ~name ~enter ~leave

let find_gsection_cache_table trace =
  let name, enter, leave = gsection_events in
  find_activity_cache trace ~name ~enter ~leave

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
    int_
    "SELECT DISTINCT(argptr) FROM events;"
  |> Array.of_list

(* Exposed functions *)

let from_sqlite_file filename =
  let db = Sql.open_file ~filename in
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
      real2
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
  let res = results trace.db real2 req in
  List.map (fun (l, u) -> Range.{ l; u; }) res

let events_between { db; procs; } ~granularity ~between ~proc ~kind () =
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
  let res = results db real req in
  let rec loop acc prev_t ts =
    match ts with
    | [] ->
       List.rev acc
    | t :: ts ->
       assert (t >= prev_t);
       if t -. prev_t < granularity
       then loop acc prev_t ts
       else loop (t :: acc) t ts
  in
  loop [] 0. res

let max_heap_size { db; _ } =
  let req =
    "SELECT max(arg1)
     FROM events
     WHERE kind = \"HEAP_OCCUPANCY\";"
  in
  match result db into req with
  | None ->
     0
  | Some i ->
     i

let heap_size_between { db; _ } ~between ~granularity () =
  let { Range.l; Range.u; } = between in
  let req =
    Printf.sprintf
      "SELECT max(time), max(arg1)
       FROM events
       WHERE kind = 'HEAP_OCCUPANCY'
       AND %f <= time AND time <= %f
       GROUP BY CAST (time / %f AS INTEGER)
       HAVING %f <= time AND time <= %f;"
      l
      u
      granularity
      l
      u
  in
  results db real2 req

let max_heap_occupancy { db; _ } =
  let req =
    "SELECT max(arg2)
     FROM events
     WHERE kind = \"HEAP_OCCUPANCY\";"
  in
  match result db into req with
  | None ->
     0
  | Some i ->
     i

let heap_occupancy_between { db; _ } ~between ~granularity () =
  let { Range.l; Range.u; } = between in
  let req =
    Printf.sprintf
      "SELECT max(time), max(arg2)
       FROM events
       WHERE kind = 'HEAP_OCCUPANCY'
       AND %f <= time AND time <= %f
       GROUP BY CAST (time / %f AS INTEGER)
       HAVING %f <= time AND time <= %f;"
      l
      u
      granularity
      l
      u
  in
  results db real2 req

let max_chunkp_size { db; _ } =
  let req =
    "SELECT max(arg1)
     FROM events
     WHERE kind = \"CHUNKP_OCCUPANCY\";"
  in
  match result db into req with
  | None ->
     0
  | Some i ->
     i

let max_chunkp_occupancy { db; _ } =
  let req =
    "SELECT max(arg2)
     FROM events
     WHERE kind = \"CHUNKP_OCCUPANCY\";"
  in
  match result db into req with
  | None ->
     0
  | Some i ->
     i

let chunkp_occupancy_between { db; _ } ~between ~granularity () =
  let { Range.l; Range.u; } = between in
  let req =
    Printf.sprintf
      "SELECT max(time), max(arg2)
       FROM events
       WHERE kind = 'CHUNKP_OCCUPANCY'
       AND %f <= time AND time <= %f
       GROUP BY CAST (time / %f AS INTEGER)
       HAVING %f <= time AND time <= %f;"
      l
      u
      granularity
      l
      u
  in
  results db real2 req

let max_ratio { db; procs } ~proc =
  try
    let req =
      Printf.sprintf
        "SELECT max(arg1/arg2)
         FROM events
         WHERE kind = 'CHUNKP_RATIO' AND argptr = %d AND arg2 > 0;"
        procs.(proc)
    in
    result db real req
  with _ ->
    0.

let ratio_between { db; procs } ~between ~proc ~granularity () =
  let req =
    Printf.sprintf
      "SELECT max(time), max(arg1/arg2)
       FROM events
       WHERE kind = 'CHUNKP_RATIO'
       AND %f <= time AND time <= %f
       AND argptr = %d and arg2 > 0
       GROUP BY CAST (time / %f AS INTEGER)
       HAVING %f <= time AND time <= %f;"
      between.Range.l
      between.Range.u
      procs.(proc)
      granularity
      between.Range.l
      between.Range.u
  in
  results db real2 req

let max_locally_collectible { db; procs; } ~proc () =
  try
    let req =
      Printf.sprintf
        "SELECT max(arg2)
         FROM events
         WHERE kind = 'CHUNKP_RATIO' AND argptr = %d;"
        procs.(proc)
    in
    result db real req
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
  results db real2 req

let max_locally_collectible_heap { db; procs; } ~proc () =
  try
    let req =
      Printf.sprintf
        "SELECT max(arg1)
         FROM events
         WHERE kind = \"HEAP_RATIO\" AND argptr = %d;"
        procs.(proc)
    in
    result db real req
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
  results db real2 req

let max_control_ratio { db; procs; } ~proc () =
  let req =
    Printf.sprintf
      "SELECT max(arg3) FROM events
       WHERE kind = \"HEAP_RATIO\" AND argptr = %d;
       "
      procs.(proc)
  in
  try result db real req with _ -> 0.

let max_copy { db; procs; } ~proc () =
  let req =
    Printf.sprintf
      "SELECT max(arg1) FROM events
       WHERE kind = \"COPY\" AND argptr = %d;
       "
      procs.(proc)
  in
  try result db real req with _ -> 0.

let copy_between
      { db; procs }
      ~between
      ~proc
      ~granularity
      () =
  (* granularity ignored for now *)
  let req =
    Printf.sprintf
      "SELECT time, arg1 FROM events
       WHERE kind = \"COPY\" AND %f <= time AND time <= %f
       AND argptr = %d
       ORDER BY time;
       "
      between.Range.l
      between.Range.u
      procs.(proc)
  in
  results db real2 req

(* Statistics *)

type proc_stats =
  {
    total_exec_time : Range.time;
    total_gc_time : Range.time;
    total_gsec_time : Range.time;
    total_mut_time : Range.time;
  }

let print_proc_stats fmt pstats =
  Format.fprintf fmt "EXEC TIME: %a@ "
    Range.print_time pstats.total_exec_time;
  Format.fprintf fmt "GC TIME: %a (%.2f%%)@ "
    Range.print_time pstats.total_gc_time
    (pstats.total_gc_time /. pstats.total_exec_time *. 100.);
  Format.fprintf fmt "GSEC TIME: %a (%.2f%%)@ "
    Range.print_time pstats.total_gsec_time
    (pstats.total_gsec_time /. pstats.total_exec_time *. 100.);
  Format.fprintf fmt "MUT TIME: %a (%.2f%%)"
    Range.print_time pstats.total_mut_time
    (pstats.total_mut_time /. pstats.total_exec_time *. 100.);
  ()

let processor_statistics ~proc trace =
  let proc_id = trace.procs.(proc) in
  let gc_table = find_gc_cache_table trace in
  let gsection_table = find_gsection_cache_table trace in

  let total_exec_time =
    let req =
      Printf.sprintf
        "SELECT max(time)-min(time)
         FROM events
         WHERE argptr = %d;"
        proc_id
    in
    result
      trace.db
      real
      req
  in

  let total_gc_time =
    let req =
      Printf.sprintf
        "SELECT sum(leave - enter)
         FROM %s
         WHERE argptr = %d;"
        gc_table
        proc_id
    in
    match result trace.db realo req with
    | None ->
       0.
    | Some f ->
       f
  in

  let total_gsec_time =
    let req =
      Printf.sprintf
        "SELECT sum(leave - enter)
         FROM %s
         WHERE argptr = %d;"
        gsection_table
        proc_id
    in
    match result trace.db realo req with
    | None ->
       0.
    | Some f ->
       f
  in

  let total_gc_nogsec_time =
    let req =
      Printf.sprintf
        "SELECT sum(gc.leave - gc.enter)
         FROM %s gc
         WHERE gc.argptr = %d
         AND NOT EXISTS (SELECT *
                         FROM %s gsec
                         WHERE gsec.argptr = gc.argptr
                         AND gsec.enter <= gc.enter
                         AND gc.leave <= gsec.leave);"
        gc_table
        proc_id
        gsection_table
    in
    match result trace.db realo req with
    | None ->
       0.
    | Some f ->
       f
  in

  let total_gsec_nogc_time =
    let req =
      Printf.sprintf
        "SELECT sum(gsec.leave - gsec.enter)
         FROM %s gsec
         WHERE gsec.argptr = %d
         AND NOT EXISTS (SELECT *
                         FROM %s gc
                         WHERE gsec.argptr = gc.argptr
                         AND gc.enter <= gsec.enter
                         AND gsec.leave <= gc.leave);"
        gsection_table
        proc_id
        gc_table
    in
    match result trace.db realo req with
    | None ->
       0.
    | Some f ->
       f
  in

  let total_mut_time =
    total_exec_time -. total_gc_nogsec_time -. total_gsec_nogc_time
  in

  {
    total_exec_time;
    total_gc_time;
    total_gsec_time;
    total_mut_time;
  }

type stats =
  {
    real_exec_time : Range.time;
    user_exec_time : Range.time;
    user_gc_time : Range.time;
    user_mut_time : Range.time;
    max_chunkp_occupancy : int;
    max_chunkp_size : int;
    per_proc_stats : proc_stats list;
  }

let print_stats
      fmt
      {
        real_exec_time;
        user_exec_time;
        user_gc_time;
        user_mut_time;
        max_chunkp_occupancy;
        max_chunkp_size;
        per_proc_stats;
      } =
  let print_proc_i i pstats =
    Format.fprintf fmt "@[@[<v 2>PROC %d:@ %a@]@]@\n"
      i
      print_proc_stats pstats
  in

  Format.fprintf fmt "REAL EXEC TIME: %a@\n"
    Range.print_time real_exec_time;
  Format.fprintf fmt "USER EXEC TIME: %a@\n"
    Range.print_time user_exec_time;
  Format.fprintf fmt "USER GC TIME: %a (%.2f%%)@\n"
    Range.print_time user_gc_time
    (user_gc_time /. user_exec_time *. 100.);
  Format.fprintf fmt "USER MUT TIME: %a (%.2f%%)@\n"
    Range.print_time user_mut_time
    (user_mut_time /. user_exec_time *. 100.);
  Format.fprintf fmt "CHUNKPOOL OCCUPANCY: %a / %a (%.2f%%)@\n"
    Utils.print_size max_chunkp_occupancy
    Utils.print_size max_chunkp_size
    (float max_chunkp_occupancy /. float max_chunkp_size *. 100.);
  List.iteri print_proc_i per_proc_stats;
  ()

let statistics trace =
  let per_proc_stats =
    let r = ref [] in
    for i = number_of_processors trace - 1 downto 0 do
      r := processor_statistics ~proc:i trace :: !r
    done;
    !r
  in
  let real_exec_time =
    result
      trace.db
      real
      "SELECT max(time)-min(time)
       FROM events;"
  in
  let user_exec_time =
    result
      trace.db
      real
      "SELECT sum(T)
       FROM (SELECT max(time)-min(time) T
             FROM events
             GROUP BY argptr);"
  in

  let user_gc_time =
    List.map (fun pstats -> pstats.total_gc_time) per_proc_stats |> Utils.sum
  in

  let user_mut_time =
    List.map (fun pstats -> pstats.total_mut_time) per_proc_stats |> Utils.sum
  in

  let max_chunkp_occupancy = max_chunkp_occupancy trace in
  let max_chunkp_size = max_chunkp_size trace in

  {
    real_exec_time;
    user_exec_time;
    user_gc_time;
    user_mut_time;
    max_chunkp_occupancy;
    max_chunkp_size;
    per_proc_stats;
  }
