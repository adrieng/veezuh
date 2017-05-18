open Sqlite3
module Table = Utils.SHashTable

(* {2 Basic types and functions} *)

type req = string

type raw_results = string option array list

type t =
  {
    db : Sqlite3.db;
    cache : raw_results Table.t;
  }

let open_file ~filename =
  let db = db_open ~mode:`NO_CREATE filename in
  let cache = Table.create 100 in
  {
    db;
    cache;
  }

(* {2 Low-level facilities} *)

let debug = ref false

let exec ~cb db req =
  if !debug then Format.eprintf "Executing @\n %s@." req;
  let res = Sqlite3.exec_no_headers ~cb db.db req in
  if !debug then Format.eprintf "Done@.";
  match res with
  | Rc.OK ->
     ()
  | err ->
     failwith @@ "Error: " ^ Rc.to_string err

let query ?(cached = true) ?(inverted = false) db req =
  let do_query () =
    let r = ref [] in
    let cb s = r := s :: !r in
    exec ~cb db req;
    if inverted then !r else List.rev !r
  in
  if cached
  then
    begin
    try
      let res = Table.find db.cache req in
      if !debug then Format.eprintf "Found result in cache for@\n %s@." req;
      res
    with Not_found ->
      if !debug then Format.eprintf "No result in cache for@\n %s@." req;
      let res = do_query () in
      Table.add db.cache req res;
      res
    end
  else
    do_query ()

let execute db req =
  exec (fun _ -> ()) db req

(* {2 High-level facilities} *)

type 'a sql_col_notnull_ty =
  | Int : int sql_col_notnull_ty
  | Real : float sql_col_notnull_ty
  | Text : string sql_col_notnull_ty

type 'a sql_col_ty =
  | Not_null : 'a sql_col_notnull_ty -> 'a sql_col_ty
  | Nullable : 'a sql_col_notnull_ty -> 'a option sql_col_ty

type 'a sql_row_ty =
  | Row_single : 'a sql_col_ty -> 'a sql_row_ty
  | Row_cons : 'a sql_col_ty * 'b sql_row_ty -> ('a * 'b) sql_row_ty

exception Ill_typed

let parse_col_notnull : type a. a sql_col_notnull_ty -> string -> a =
  fun ty s ->
  match ty with
  | Int ->
     int_of_string s
  | Real ->
     float_of_string s
  | Text ->
     s

let parse_col : type a. a sql_col_ty -> string option -> a =
  fun ty so ->
  match ty, so with
  | Not_null _, None ->
     raise Ill_typed
  | Not_null ty, Some s ->
     parse_col_notnull ty s
  | Nullable ty, None ->
     None
  | Nullable ty, Some s ->
     Some (parse_col_notnull ty s)

let parse_row ty (arr : string option array) =
  let rec walk : type a. a sql_row_ty -> int -> a =
  fun ty_row next ->
  match ty_row with
  | Row_single ty_col ->
     parse_col ty_col arr.(next)
  | Row_cons (ty_col, ty_row) ->
     let a = parse_col ty_col arr.(next) in
     let b = walk ty_row (next + 1) in
     (a, b)
  in
  walk ty 0

let results : type a. t -> a sql_row_ty -> string -> a list =
  fun db ty req ->
  let res = query ~cached:true ~inverted:true db req in
  List.rev_map (parse_row ty) res

let result db ty req =
  match results db ty req with
  | [] ->
     raise Ill_typed
  | x :: _ ->
     x

(* {2 Convenience values} *)

let text =
 Row_single (Not_null Text)

let int_ =
 Row_single (Not_null Int)

let into =
 Row_single (Nullable Int)

let real =
 Row_single (Not_null Real)

let realo =
 Row_single (Nullable Real)

let real2 =
 Row_cons (Not_null Real, Row_single (Not_null Real))
