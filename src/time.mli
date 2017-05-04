type time = float

type span = { l : time; u : time; }

val print_span : Format.formatter -> span -> unit

val truncate : span -> time -> time

val clip : within:span -> span -> span

val range : span -> time
