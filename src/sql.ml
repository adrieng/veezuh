include Sqlite3

let debug = ref false

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

let results : type a. db -> a sql_row_ty -> string -> a list =
  fun db ty req ->
  let r = ref [] in
  let cb s = r := s :: !r in
  if !debug then Format.eprintf "Executing request:@\n %s@." req;
  let res = exec_no_headers db ~cb req in
  if !debug then Format.eprintf "Done.@.";
  match res with
  | Rc.OK ->
     List.rev_map (parse_row ty) !r
  | err ->
     Format.eprintf "SQLite error %s executing@\n %s@."
       (Rc.to_string err)
       req;
     []
  | exception exn ->
     Format.eprintf "Exception %s executing@\n %s@."
       (Printexc.to_string exn)
       req;
     []

let result db ty req =
  List.hd @@ results db ty req

let exec_check db req =
  if !debug then Format.eprintf "Executing request:@\n %s@." req;
  let res = exec db req in
  if !debug then Format.eprintf "Done@.";
  match res with
  | Rc.OK ->
     ()
  | err ->
     failwith @@ "exec_check: " ^ Rc.to_string err
