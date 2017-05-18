type time = float

let print_time fmt t =
  let m, u = Utils.find_good_unit_scaling t in
  Format.fprintf fmt "%.2f %ss" (t *. m) u

type span = { l : time; u : time; }

let print_span fmt { l; u; } =
  Format.fprintf fmt "[%f - %f]" l u

let truncate s t =
  min (max s.l t) s.u

let range s =
  abs_float (s.l -. s.u)

let canonicalize { l; u; } =
  { l = min l u; u = max l u; }

let discrete t =
  { l = t; u = t; }

let union { l = l1; u = u1; } { l = l2; u = u2; } =
  { l = min l1 l2; u = max u1 u2; }

let intersection { l = l1; u = u1; } { l = l2; u = u2; } =
  { l = max l1 l2; u = min u1 u2; }
