type time = float

type span = { l : time; u : time; }

let print_span fmt { l; u; } =
  Format.fprintf fmt "[%f - %f]" l u

let truncate s t =
  min (max s.l t) s.u

let clip ~within s =
  { l = max within.l s.l; u = min within.u s.u; }

let range s =
  abs_float (s.l -. s.u)
