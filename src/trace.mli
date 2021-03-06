type t

val from_sqlite_file : string -> t

val purge : t -> unit

val prepare : ?verbose:bool -> t -> unit

val epoch : t -> Range.span

val number_of_processors : t -> int

val signal_between :
  t ->
  kind:string ->
  selector:string ->
  between:Range.span ->
  granularity:Range.time ->
  proc:int ->
  unit ->
  (float * float) list

val activities_between :
  t ->
  name:string ->
  enter:string ->
  leave:string ->
  between:Range.span ->
  min_duration:float ->
  proc:int ->
  unit ->
  Range.span list

val events_between :
  t ->
  granularity:Range.time ->
  between:Range.span ->
  proc:int ->
  kind:string ->
  unit ->
  float list

val max_heap_size :
  t ->
  int

val heap_size_between :
  t ->
  between:Range.span ->
  granularity:float ->
  unit ->
  (float * float) list

val max_heap_occupancy :
  t ->
  int

val heap_occupancy_between :
  t ->
  between:Range.span ->
  granularity:float ->
  unit ->
  (float * float) list

val max_chunkp_size :
  t ->
  int

val max_chunkp_occupancy :
  t ->
  int

val chunkp_occupancy_between :
  t ->
  between:Range.span ->
  granularity:float ->
  unit ->
  (float * float) list

val max_array_mem :
  t ->
  int

val array_mem_between :
  t ->
  between:Range.span ->
  granularity:float ->
  unit ->
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
  unit ->
  (float * float) list

val max_locally_collectible :
  t ->
  proc:int ->
  unit ->
  float

val locally_collectible_between :
  t ->
  between:Range.span ->
  proc:int ->
  granularity:float ->
  unit ->
  (float * float) list

val max_locally_collectible_heap :
  t ->
  proc:int ->
  unit ->
  float

val locally_collectible_heap_between :
  t ->
  between:Range.span ->
  proc:int ->
  granularity:float ->
  unit ->
  (float * float) list

val max_control_ratio :
  t ->
  proc:int ->
  unit ->
  float

val max_copy :
  t ->
  proc:int ->
  unit ->
  float

(* Statistics *)

type proc_stats =
  {
    total_exec_time : Range.time;
    total_gc_time : Range.time;
    total_gsec_time : Range.time;
    total_mut_time : Range.time;
    total_bytes_copied : int;
  }

val print_proc_stats : Format.formatter -> proc_stats -> unit

val processor_statistics : proc:int -> t -> proc_stats

type stats =
  {
    real_exec_time : Range.time;
    user_exec_time : Range.time;
    user_gc_time : Range.time;
    user_mut_time : Range.time;
    max_heap_occupancy : int;
    max_heap_size : int;
    max_chunkp_occupancy : int;
    max_chunkp_size : int;
    max_bytes_copied : int;
    per_proc_stats : proc_stats list;
  }

val statistics : t -> stats

val print_stats : Format.formatter -> stats -> unit
