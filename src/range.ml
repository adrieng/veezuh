type time = float

let print_time fmt t =
  let m, u = Utils.find_good_unit_scaling t in
  Format.fprintf fmt "%.2f %s" (t *. m) u

type span = { l : time; u : time; }

let print_span fmt { l; u; } =
  Format.fprintf fmt "[%f - %f]" l u

let truncate s t =
  min (max s.l t) s.u

let clip ~within s =
  { l = max within.l s.l; u = min within.u s.u; }

let range s =
  abs_float (s.l -. s.u)

let canonicalize { l; u; } =
  { l = min l u; u = max l u; }
