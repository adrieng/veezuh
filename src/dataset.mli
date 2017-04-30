type dataset

val from_sqlite_file : string -> dataset

val time_range : dataset -> float * float

val number_of_processors : dataset -> int
