open Sql

type dataset = db

let tables db =
  results
    db
    Text
    "SELECT name FROM sqlite_master WHERE type = 'table';"

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

  db

let time_range db =
  result
    db
    (Pair (Real, Real))
    "SELECT MIN(time), MAX(time) FROM events;"

let number_of_processors db =
  result
    db
    Int
    "SELECT COUNT(*) FROM events WHERE kind = 'INIT';"
