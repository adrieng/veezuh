type time = float

val print_time : Format.formatter -> time -> unit

type span = { l : time; u : time; }

val print_span : Format.formatter -> span -> unit

val truncate : span -> time -> time

val clip : within:span -> span -> span

val range : span -> time

val canonicalize : span -> span
