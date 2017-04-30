open Sql

type dataset = db

let tables db =
  results
    db
    Text
    "SELECT name FROM sqlite_master WHERE type = 'table';"

let from_sqlite_file filename =
  let db = db_open ~mode:`NO_CREATE filename in
  let tables = tables db in

  if not @@ Sequence.mem "events" tables
  then failwith (filename ^ ": missing event table");

  db

let time_range db =
  let bounds =
    results
      db
      (Pair (Real, Real))
      "SELECT MIN(time), MAX(time) FROM events;"
  in
  Sequence.head_exn bounds

let number_of_processors db =
  let procs =
    results
      db
      Int
      "SELECT COUNT(*) FROM events WHERE kind = 'INIT';"
  in
  Sequence.head_exn procs
