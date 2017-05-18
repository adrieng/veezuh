type time = float

val print_time : Format.formatter -> time -> unit

type span = { l : time; u : time; }

val print_span : Format.formatter -> span -> unit

val truncate : span -> time -> time

val range : span -> time

val canonicalize : span -> span

val discrete : time -> span

val union : span -> span -> span

val intersection : span -> span -> span
