type t

val from_sqlite_file : string -> t

val epoch : t -> Range.span

val number_of_processors : t -> int

val activities_between :
  kind:string ->
  between:Range.span ->
  min_duration:float ->
  proc:int ->
  t ->
  Range.span list

val events_between :
  between:Range.span ->
  proc:int ->
  kind:string ->
  t ->
  float list

val max_occupancy :
  t ->
  float

val occupancy_between :
  between:Range.span ->
  granularity:float ->
  t ->
  (float * float) list
