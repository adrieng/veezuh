type time = float

type span = { l : time; u : time; }

let truncate s t =
  min (max s.l t) s.u

let range s =
  abs_float (s.l -. s.u)
