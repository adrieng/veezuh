type t

val from_sqlite_file : string -> t

val epoch : t -> Time.span

val number_of_processors : t -> int

val gc_periods_between :
  between:Time.span ->
  min_duration:float ->
  proc:int ->
  t ->
  Time.span list

val activities_between :
  kind:string ->
  between:Time.span ->
  min_duration:float ->
  proc:int ->
  t ->
  Time.span list

val events_between :
  between:Time.span ->
  proc:int ->
  kind:string ->
  t ->
  Time.time list
