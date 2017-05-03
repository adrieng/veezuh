(* Time-related primitives *)

type time = float

type time_span = float * float

val truncate : time_span -> time -> time

(* Activities and Events *)

type activity_kind = string

type event_kind = string

type processor = int

(* Callbacks *)

type get_activities_callback =
  activity:activity_kind ->
  for_proc:processor ->
  between:time_span ->
  time_span list

type get_events_callback =
  event:event_kind ->
  for_proc:processor ->
  between:time_span ->
  time list

(* The Timeline control *)

type t

val make :
  width:int ->
  height:int ->
  number_of_processors:int ->
  packing:(GObj.widget -> unit) ->
  t
