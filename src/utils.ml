(* Math stuff *)

let pi = 4. *. atan 1.
let pi2 = 2. *. pi

let middle x1 x2 =
  (x1 +. x2) /. 2.

(* Color stuff *)

type rgb = float * float * float

type rgba = float * float * float * float

let white_rgb = 1., 1., 1.

let set_rgba cr (r, g, b, a) =
  Cairo.set_source_rgba cr ~r ~g ~b ~a

let set_rgb cr (r, g, b) =
  Cairo.set_source_rgba cr ~r ~g ~b ~a:1.

let set_black cr =
  set_rgb cr (0., 0., 0.)

let gray_rect ~x ~y ~width ~height cr =
  Cairo.set_source_rgb cr 0.5 0.5 0.5;
  Cairo.rectangle cr x y width height;
  Cairo.fill cr;
  ()

(* Misc stuff *)

let find_good_unit_scaling t =
  let rec find t p =
    if p >= 3 then t, p
    else if t < 0.01 then find (t *. 1000.) (p + 1)
    else t, p
  in
  let _, p = find t 0 in
  1000. ** float p, List.nth ["s"; "ms"; "us"; "ns"] p
