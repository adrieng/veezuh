open Sql

type t =
  {
    db : db;
    procs : int array;
  }

type proc_id = int

let tables db =
  results
    db
    Text
    "SELECT name FROM sqlite_master WHERE type = 'table';"

let processor_ids db =
  results
    db
    Int
    "SELECT DISTINCT(argptr) FROM events;"
  |> Array.of_list

let purge db =
  let keep = [ "events" ] in
  let drop s =
    exec_check
      db
      (Printf.sprintf "DROP TABLE %s;" s)
  in
  List.(tables db
        |> filter (fun s -> not @@ List.mem s keep)
        |> iter drop)

let from_sqlite_file filename =
  let db = db_open ~mode:`NO_CREATE filename in
  let tables = tables db in

  if not @@ List.mem "events" tables
  then failwith (filename ^ ": missing event table");

  let procs = processor_ids db in
  if Array.length procs = 0
  then failwith (filename ^ ": no events");

  { db; procs; }

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

let activities_between { db; procs; } ~kind ~between ~min_duration ~proc =
  let req =
    Printf.sprintf
      "SELECT e.time, l.time
       FROM events e JOIN events l
       WHERE e.kind = \"%s_ENTER\" AND l.kind = \"%s_LEAVE\"
       AND e.argptr = l.argptr AND e.argptr = %d
       AND l.time - e.time >= %f
       AND ((%f <= e.time AND e.time <= %f)
            OR (%f <= l.time AND l.time <= %f)
            OR (e.time <= %f AND l.time >= %f))
       AND NOT EXISTS
       (SELECT * FROM events b
        WHERE b.kind = \"GC_LEAVE\" AND e.time < b.time
        AND b.time < l.time);"
      kind kind
      (procs.(proc))
      min_duration
      between.Range.l
      between.Range.u
      between.Range.l
      between.Range.u
      between.Range.l
      between.Range.u
  in
  let res = results db (Pair (Real, Real)) req in
  List.map (fun (l, u) -> Range.{ l; u; }) res

let events_between { db; procs; } ~between ~proc ~kind =
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

let occupancy_between { db; _ } ~between ~granularity =
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

let ratio_between { db; procs } ~between ~proc ~granularity =
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
