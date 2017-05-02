open Cairo

let pi = 4. *. atan 1.
let pi2 = 2. *. pi

let find_good_unit_prefix t =
  assert (t >= 0.);
  let rec find t p =
    if p <= -3 || p >= 2 then t, p
    else if t <= 1. then find (t *. 1000.) (p - 1)
    else if t >= 60. then find (t /. 60.) (p + 1)
    else t, p
  in
  let t, p = find t 0 in
  t, List.nth ["ns"; "us"; "ms"; "s"; "m"; "h"] (p + 3)
;;

let draw_scale_bar
      ~number_of_increments
      ~thickness
      ~increments_height
      ~width
      ~height
      ~cur_min_time
      ~cur_max_time
      cr =
  assert (height >= thickness /. 2. +. increments_height);

  (* Move so that 0 is verticaly centered. *)
  Cairo.translate cr 0. (height /. 2.);

  (* Draw the main axis. *)
  Cairo.set_line_width cr thickness;
  Cairo.move_to cr 0. 0.;
  Cairo.line_to cr width 0.;
  Cairo.stroke cr;

  (* Draw the increments *)

  let pixels_per_increment = width /. float number_of_increments in
  let total_time = cur_max_time -. cur_min_time in
  let time_per_increment = total_time /. float number_of_increments in

  let draw_increment ?(label = "") x =
    let y = -. increments_height -. thickness in
    (* Draw increment bar *)
    Cairo.move_to cr x 0.;
    Cairo.line_to cr x y;
    Cairo.stroke cr;
    (* Draw increment label, if any *)
    Cairo.move_to cr (x +. 2.) (-. 5.);
    Cairo.Path.text cr label;
    Cairo.stroke cr;
  in

  Cairo.set_line_width cr 1.;
  for i = 0 to number_of_increments - 1 do
    let x = float i *. pixels_per_increment in
    (* Draw small label *)
    let x_time = float i *. time_per_increment in
    let x_time, pref = find_good_unit_prefix x_time in
    let label = Printf.sprintf "%.2f %s" x_time pref in
    draw_increment ~label x;
  done;

  draw_increment width;

  ()

class timeline ~packing trace =
  let pcount = Trace.number_of_processors trace in

  let min_time, max_time = Trace.time_range trace in

  let da = GMisc.drawing_area ~packing:packing () in

  object (self)
    inherit GObj.widget da#as_widget

    val mutable cur_min_time = min_time

    val mutable cur_max_time = max_time

    val scale_bar_number_of_increments = 10

    val scale_bar_thickness = 1.

    val scale_bar_increments_height = 4.

    val scale_bar_height = 40.

    method expose () =
      let cr = Cairo_gtk.create da#misc#window in
      let al = da#misc#allocation in
      let width = float al.Gtk.width in
      let height = float al.Gtk.height in

      let scale_bar_width = width *. 0.8 in
      let scale_bar_offset = (width -. scale_bar_width) /. 2. in
      Cairo.translate cr scale_bar_offset 0.;
      draw_scale_bar
        ~number_of_increments:scale_bar_number_of_increments
        ~thickness:scale_bar_thickness
        ~increments_height:scale_bar_increments_height
        ~width:scale_bar_width
        ~height:scale_bar_height
        ~cur_min_time
        ~cur_max_time
        cr;

      Cairo.identity_matrix cr;

      ()

    initializer
    let callback _ = self#expose (); false in
    ignore @@ da#event#connect#expose ~callback;
    Printf.printf "Timelines initialized.\n"
  end
