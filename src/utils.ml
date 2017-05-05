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

let rgb_byte_of_rgb_float (r, g, b) =
  let f x = min 255 (int_of_float (x *. 255.)) in
  (f r, f g, f b)

let rgb_float_of_rgb_byte (r, g, b) =
  let f x = float x /. 255. in
  (f r, f g, f b)

let next_color =
  let i = ref 0 in
  let colors =
    Array.map
      rgb_float_of_rgb_byte
      [|
        (* http://sashat.me/2017/01/11/list-of-20-simple-distinct-colors/ *)
        (170, 110, 40);
        (130, 0, 150);
        (255, 205, 80);
        (128, 128, 0);
        (255, 235, 0);
        (255, 250, 200);
        (190, 255, 0);
        (170, 255, 195);
        (0, 128, 128);
        (255, 150, 0);
        (255, 200, 220);
        (0, 190, 0);
        (100, 255, 255);
        (0, 0, 128);
        (67, 133, 255);
        (230, 190, 255);
        (255, 0, 255);
        (128, 128, 128);
      |]
  in
  fun () ->
  let c = colors.(!i) in
  incr i;
  if !i >= Array.length colors then i := 0;
  c

let gdk_color_of_rgb_float (r, g, b) =
  let f x = int_of_float (x *. 65535.) in
  let r, g, b = f r, f g, f b in
  GDraw.color (`RGB (r, g, b))

(* Misc stuff *)

let find_good_unit_scaling t =
  let rec find t p =
    if p >= 3 then t, p
    else if t < 0.01 then find (t *. 1000.) (p + 1)
    else t, p
  in
  let _, p = find t 0 in
  1000. ** float p, List.nth ["s"; "ms"; "us"; "ns"] p

let get_opt o =
  match o with
  | None ->
     failwith "get_opt"
  | Some x ->
     x
