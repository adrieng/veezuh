type t

val from_sqlite_file : string -> t

val epoch : t -> Range.span

val number_of_processors : t -> int

val gc_periods_between :
  between:Range.span ->
  min_duration:float ->
  proc:int ->
  t ->
  Range.span list

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
  Range.time list
