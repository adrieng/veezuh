(* {2 Basic types and functions} *)

type t

type req = string

val open_file : filename:string -> t

(* {2 Low-level facilities} *)

val debug : bool ref

val exec_check : t -> req -> unit

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

val results : t -> 'a sql_row_ty -> req -> 'a list

val result : t -> 'a sql_row_ty -> req -> 'a

(* {2 Convenience values} *)

val text : string sql_row_ty

val int_ : int sql_row_ty

val into : int option sql_row_ty

val real : float sql_row_ty

val realo : float option sql_row_ty

val real2 : (float * float) sql_row_ty
