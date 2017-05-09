(* Keys *)

type signal_max_callback =
  unit ->
  float

type signal_samples_callback =
  between:Range.span ->
  granularity:float ->
  (float * float) list

type activity_callback =
  between:Range.span ->
  min_duration:Range.time ->
  Range.span list

type event_callback =
  between:Range.span ->
  Range.time list

type key_kind =
  | Signal of signal_kind
  | Activity of activity_kind
  | Event of event_kind

and signal_kind =
  {
    max : signal_max_callback;
    samples : signal_samples_callback;
    alpha_mult : float;
  }

and activity_kind =
  {
    activities : activity_callback;
  }

and event_kind =
  {
    events : event_callback;
  }

type key_name = string

type key

val make_key :
  name:key_name ->
  kind:key_kind ->
  color:Utils.rgba ->
  visible:bool ->
  key

val get_key_name : key -> key_name

val get_key_kind : key -> key_kind

val get_key_color : key -> Utils.rgba

val get_key_visible : key -> bool

val set_key_visible : key -> visible:bool -> unit

(* Rows *)

type row_name = string

type row

val make_row :
  name:row_name ->
  rank:int ->
  height:int ->
  background:Utils.rgba ->
  visible:bool ->
  row

val get_row_name : row -> row_name

val get_row_height : row -> int

val get_row_background : row -> Utils.rgba

val get_row_visible : row -> bool

val set_row_visible : row -> visible:bool -> unit

val row_add_key : row -> key -> unit

val row_find_key : row -> key_name -> key

val row_iter_keys : (key -> unit) -> row -> unit

val row_fold_keys : ('a -> key -> 'a) -> 'a -> row -> 'a

(* The Timeline control *)

type t

val make :
  global_epoch:Range.span ->
  packing:(GObj.widget -> unit) ->
  unit ->
  t

val refresh : t -> unit

val add_row : t -> row -> unit

val find_row : t -> row_name -> row

val iter_rows : (row -> unit) -> t -> unit

val fold_rows : ('a -> row -> 'a) -> 'a -> t -> 'a

val zoom_to_global : t -> unit

val zoom_to_selection : t -> unit
