type time = float

type time_span = float * float

let truncate (t_min, t_max) t =
  min (max t_min t) t_max

type activity_kind = string

type event_kind = string

type processor = int

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

type t = unit

let make ~width ~height ~number_of_processors ~packing =
  ()
