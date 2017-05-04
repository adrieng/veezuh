(* Activities and Events *)

type activity_kind = string

type event_kind = string

type processor = int

(* Callbacks *)

type get_activities_callback =
  kind:activity_kind ->
  for_proc:processor ->
  between:Time.span ->
  min_duration:Time.time ->
  Time.span list

type get_events_callback =
  kind:event_kind ->
  for_proc:processor ->
  between:Time.span ->
  Time.time list

(* The Timeline control *)

type t

val make :
  global_epoch:Time.span ->
  number_of_processors:int ->
  get_activities:get_activities_callback ->
  get_events:get_events_callback ->
  packing:(GObj.widget -> unit) ->
  unit ->
  t

val add_activity : kind:activity_kind -> color:Utils.rgba -> t -> unit

val add_event : kind:event_kind -> color:Utils.rgba -> t -> unit

val zoom_to_global : t -> unit

val zoom_to_selection : t -> unit
