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

let rgba cr (r, g, b, a) =
  Cairo.set_source_rgba cr ~r ~g ~b ~a

let black cr =
  Cairo.set_source_rgb cr 0. 0. 0.

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

  black cr;

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
;;

let draw_processor_chart
      ?(color_user = (0., 1., 0.4, 1.))
      ?(color_gc = (0.7, 0., 0.2, 1.))
      ~width
      ~height
      ~cur_min_time
      ~cur_max_time
      ~processor
      ~trace
      cr =
  (* Draw the "Processor XXX" label. *)
  let label = "Processor " ^ string_of_int processor in
  let label_extent = Cairo.text_extents cr label in
  black cr;
  Cairo.move_to
    cr
    (-. label_extent.width -. 5.)
    (height /. 2. +. label_extent.height /. 2.);
  Cairo.Path.text cr label;
  Cairo.stroke cr;

  (* By default, a processor is considered active. *)
  rgba cr color_user;
  Cairo.rectangle cr 0. 0. width height;
  Cairo.fill cr;

  (* Draw GC period *)
  rgba cr color_gc;

  let total_time = cur_max_time -. cur_min_time in
  let x_ratio = width /. total_time in

  let draw_gc_period (start, stop) =
    (* start and stop are in absolute time, we have to translate them first to
       local time, then to x coordinates. *)
    let l_start = start -. cur_min_time in
    let l_stop = stop -. cur_min_time in
    let x_start = l_start *. x_ratio in
    let x_stop = l_stop *. x_ratio in
    let width = max (x_stop -. x_start) 1. in
    Cairo.rectangle cr x_start 0. width height;
    Cairo.fill cr
  in

  let gc_periods =
    Trace.gc_periods_between
      ~min:cur_min_time
      ~max:cur_max_time
      ~proc:processor
      trace
  in
  List.iter draw_gc_period gc_periods;

  ()
;;

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

    val proc_chart_horizontal_spacing = 5.

    val proc_chart_max_height = 30.

    method expose () =
      let cr = Cairo_gtk.create da#misc#window in
      let al = da#misc#allocation in
      let width = float al.Gtk.width in
      let height = float al.Gtk.height in

      let chart_width = width *. 0.9 in
      let chart_offset = (width -. chart_width) *. 0.9 in

      (* Draw the scale bar *)
      Cairo.translate cr chart_offset 0.;
      draw_scale_bar
        ~number_of_increments:scale_bar_number_of_increments
        ~thickness:scale_bar_thickness
        ~increments_height:scale_bar_increments_height
        ~width:chart_width
        ~height:scale_bar_height
        ~cur_min_time
        ~cur_max_time
        cr;

      (* Draw each processor's chart *)
      let proc_chart_height =
        min ((height -. scale_bar_height) /. float pcount) proc_chart_max_height
      in
      for p = 0 to pcount - 1 do
        Cairo.translate cr 0. proc_chart_horizontal_spacing;
        draw_processor_chart
          ~width:chart_width
          ~height:proc_chart_height
          ~cur_min_time
          ~cur_max_time
          ~processor:p
          ~trace
          cr;
        Cairo.translate cr 0. proc_chart_height;
      done;
      Cairo.identity_matrix cr;

      ()

    initializer
    let callback _ = self#expose (); false in
    ignore @@ da#event#connect#expose ~callback;
    Printf.printf "Timelines initialized.\n"
  end
