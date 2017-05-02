open Sql

type t =
  {
    db : db;
    procs : int array;
  }

type proc_id = int

let debug = ref false

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

let time_range { db; _ } =
  result
    db
    (Pair (Real, Real))
    "SELECT MIN(time), MAX(time) FROM events;"

let number_of_processors { procs; _ } =
  Array.length procs

let gc_periods_between ~min ~max ~proc { db; procs; } =
  if !debug then
    Printf.eprintf
      "Querying for GC activty in [%f,%f] on proc %d\n"
      min
      max
      proc;
  let req =
    Printf.sprintf
      "
       SELECT e.time, l.time
       FROM events e JOIN events l
       WHERE e.kind = \"GC_ENTER\" AND l.kind = \"GC_LEAVE\"
       AND e.argptr = l.argptr AND e.argptr = %d AND e.time < l.time
       AND %f <= e.time AND l.time <= %f
       AND NOT EXISTS
       (SELECT * FROM events b
        WHERE b.kind = \"GC_LEAVE\" AND e.time < b.time
        AND b.time < l.time);"
      (procs.(proc))
      min
      max
  in
  let l =
    results
      db
      (Pair (Real, Real))
      req
  in
  if !debug then Printf.eprintf "=> Got %d GC periods\n" (List.length l);
  l
