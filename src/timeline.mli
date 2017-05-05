(* Activities and Events *)

type activity_kind = string

type event_kind = string

type processor = int

(* Callbacks *)

type get_activities_callback =
  kind:activity_kind ->
  for_proc:processor ->
  between:Range.span ->
  min_duration:Range.time ->
  Range.span list

type get_events_callback =
  kind:event_kind ->
  for_proc:processor ->
  between:Range.span ->
  Range.time list

(* The Timeline control *)

type t

val make :
  global_epoch:Range.span ->
  number_of_processors:int ->
  get_activities:get_activities_callback ->
  get_events:get_events_callback ->
  packing:(GObj.widget -> unit) ->
  unit ->
  t

val add_activity : kind:activity_kind -> color:Utils.rgba -> t -> unit

val remove_activity : kind:activity_kind -> t -> unit

val add_event : kind:event_kind -> color:Utils.rgba -> t -> unit

val remove_event : kind:event_kind -> t -> unit

val zoom_to_global : t -> unit

val zoom_to_selection : t -> unit
