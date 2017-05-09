include Sqlite3

let debug = ref true

type 'a sqlty =
  | Int : int sqlty
  | Real : float sqlty
  | Text : string sqlty
  | Pair : 'a sqlty * 'b sqlty -> ('a * 'b) sqlty

let parse ty (arr : string array) =
  let rec walk : type a. a sqlty -> int -> a * int =
  fun ty next ->
  let get () =
    if next >= Array.length arr
    then
      invalid_arg
        (Printf.sprintf "parse: trying to access index %d of array of length %d"
           next (Array.length arr))
    else
      arr.(next)
  in
  match ty with
  | Int ->
     int_of_string (get ()),
     next + 1
  | Real ->
     float_of_string (get ()),
     next + 1
  | Text ->
     arr.(next),
     next + 1
  | Pair (ty1, ty2) ->
     let r1, next = walk ty1 next in
     let r2, next = walk ty2 next in
     (r1, r2), next
  in

  let r, next = walk ty 0 in
  if next <> Array.length arr
  then
    invalid_arg
      (Printf.sprintf "parse: expected %d fields, got %d fields"
        next
        (Array.length arr))
  ;
  r

let results : type a. db -> a sqlty -> string -> a list =
  fun db ty req ->
  let r = ref [] in
  let cb s = r := s :: !r in
  if !debug then Format.eprintf "Executing request:@\n %s@." req;
  let res = exec_not_null_no_headers db ~cb req in
  if !debug then Format.eprintf "Done.@.";
  match res with
  | Rc.OK ->
     List.rev_map (parse ty) !r
  | err ->
     Format.eprintf "SQLite error %s executing %s@."
       (Rc.to_string err)
       req;
     []
  | exception exn ->
     Format.eprintf "Exception %s executing %s@."
       (Printexc.to_string exn)
       req;
     []

let result db ty req =
  List.hd @@ results db ty req

let exec_check db req =
  match exec db req with
  | Rc.OK ->
     ()
  | err ->
     failwith @@ "exec_check: " ^ Rc.to_string err
