type time = float

type span = { l : time; u : time; }

val truncate : span -> time -> time

val range : span -> time
