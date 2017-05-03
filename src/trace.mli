type t

val from_sqlite_file : string -> t

val time_range : t -> float * float

val number_of_processors : t -> int

val gc_periods_between :
  min:float ->
  max:float ->
  min_duration:float ->
  proc:int ->
  t ->
  (float * float) list

val events_between :
  min:float ->
  max:float ->
  proc:int ->
  kind:string ->
  t ->
  float list
