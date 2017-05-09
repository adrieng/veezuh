type t

val from_sqlite_file : string -> t

val epoch : t -> Range.span

val number_of_processors : t -> int

val activities_between :
  t ->
  kind:string ->
  between:Range.span ->
  min_duration:float ->
  proc:int ->
  Range.span list

val events_between :
  t ->
  between:Range.span ->
  proc:int ->
  kind:string ->
  float list

val max_occupancy :
  t ->
  float

val occupancy_between :
  t ->
  between:Range.span ->
  granularity:float ->
  (float * float) list

val max_ratio :
  t ->
  proc:int ->
  float

val ratio_between :
  t ->
  between:Range.span ->
  proc:int ->
  granularity:float ->
  (float * float) list

val max_locally_collectible :
  t ->
  proc:int ->
  float

val locally_collectible_between :
  t ->
  between:Range.span ->
  proc:int ->
  granularity:float ->
  (float * float) list

val max_locally_collectible_heap :
  t ->
  proc:int ->
  float

val locally_collectible_heap_between :
  t ->
  between:Range.span ->
  proc:int ->
  granularity:float ->
  (float * float) list
