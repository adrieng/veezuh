open Cairo

let pi = 4. *. atan 1.
let pi2 = 2. *. pi

let find_good_unit_prefix t =
  assert (t >= 0.);
  let rec find t p =
    if p >= 3 then t, p
    else if t < 0.01 then find (t *. 1000.) (p + 1)
    else t, p
  in
  let t, p = find t 0 in
  t, List.nth ["s"; "ms"; "us"; "ns"] p
;;

let rgba cr (r, g, b, a) =
  Cairo.set_source_rgba cr ~r ~g ~b ~a

let black cr =
  Cairo.set_source_rgb cr 0. 0. 0.

let gray_rect ~x ~y ~width ~height cr =
  Cairo.set_source_rgb cr 0.5 0.5 0.5;
  Cairo.rectangle cr 0. 0. width height;
  Cairo.fill cr;
  ()

let draw_background
      ~width
      ~height
      cr =
  Cairo.set_source_rgb cr 1. 1. 1.;
  Cairo.rectangle cr 0. 0. width height;
  Cairo.fill cr;
  ()

let draw_scale_bar
      ~number_of_increments
      ~thickness
      ~increments_height
      ~width
      ~height
      ~min_time
      ~cur_min_time
      ~cur_max_time
      cr =
  Cairo.translate cr 0. (height -. 1.);

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
    let x_time = float i *. time_per_increment +. cur_min_time -. min_time in
    let x_time, pref = find_good_unit_prefix x_time in
    let label = Printf.sprintf "%.2f %s" x_time pref in
    draw_increment ~label x;
  done;

  draw_increment width;

  ()
;;

let draw_processor_chart
      ?(color_user = (0., 1., 0.2, 0.2))
      ?(color_gc = (1., 0., 0.5, 1.))
      ~width
      ~height
      ~cur_min_time
      ~cur_max_time
      ~processor
      ~trace
      cr =
  (* Draw the "P XXX" label. *)
  let label = "P" ^ string_of_int processor in
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

  let sw = GBin.scrolled_window ~packing:packing () in

  let da = GMisc.drawing_area ~packing:sw#add_with_viewport () in

  object (self)
    inherit GObj.widget da#as_widget

    val mutable cur_min_time = min_time

    val mutable cur_max_time = max_time

    method cur_time_span () =
      cur_max_time -. cur_min_time

    val scroll_zoom_factor = 0.05

    method zoom_to new_min_time new_max_time =
      cur_min_time <- max min_time new_min_time;
      cur_max_time <- min max_time new_max_time;
      GtkBase.Widget.queue_draw da#as_widget

    method zoom_restore () =
      self#zoom_to min_time max_time

    method zoom_adjust x_prop zoom_factor =
      let adjustment = zoom_factor *. self#cur_time_span () in

      let left_adjustment =
        if x_prop <= 0.2 then 0. else adjustment *. x_prop
      in
      let right_adjustment =
        if x_prop >= 0.8 then 0. else adjustment *. (1. -. x_prop)
      in

      let new_min_time = cur_min_time +. left_adjustment in
      let new_max_time = cur_max_time -. right_adjustment in
      self#zoom_to new_min_time new_max_time

    method zoom_in x_prop =
      self#zoom_adjust x_prop scroll_zoom_factor

    method zoom_out x_prop =
      self#zoom_adjust x_prop (-. scroll_zoom_factor)

    val scale_bar_number_of_increments = 10

    val scale_bar_thickness = 1.

    val scale_bar_increments_height = 4.

    val scale_bar_height = 20.

    val proc_chart_horizontal_spacing = 2.

    val proc_chart_height = 30.

    val mutable width = 0.

    val mutable height = 0.

    method height () =
      let per_processor_height =
        proc_chart_height +. proc_chart_horizontal_spacing
      in
      scale_bar_height +. float pcount *. per_processor_height

    method chart_width () =
      width *. 0.9

    method chart_x_min () =
      (width -. self#chart_width ()) *. 0.9

    method chart_x_max () =
      self#chart_x_min () +. self#chart_width ()

    method clamp_x_to_chart x =
      min (max x (self#chart_x_min ())) (self#chart_x_max ())

    method width () =
      let al = sw#misc#allocation in
      float (al.Gtk.width - 10)

    method on_expose () =
      let cr = Cairo_gtk.create da#misc#window in
      let al = da#misc#allocation in
      width <- float al.Gtk.width;
      height <- float al.Gtk.height;

      let chart_width = self#chart_width () in
      let chart_offset = self#chart_x_min () in

      (* Draw background *)
      draw_background
        ~width
        ~height
        cr;

      (* Draw the scale bar *)
      Cairo.translate cr chart_offset 0.;
      draw_scale_bar
        ~number_of_increments:scale_bar_number_of_increments
        ~thickness:scale_bar_thickness
        ~increments_height:scale_bar_increments_height
        ~width:chart_width
        ~height:scale_bar_height
        ~min_time
        ~cur_min_time
        ~cur_max_time
        cr;

      Cairo.identity_matrix cr;
      Cairo.translate cr chart_offset scale_bar_height;

      (* Draw each processor's chart *)
      for p = 0 to pcount - 1 do
        Cairo.translate ~x:0. ~y:proc_chart_horizontal_spacing cr;
        draw_processor_chart
          ~width:chart_width
          ~height:proc_chart_height
          ~cur_min_time
          ~cur_max_time
          ~processor:p
          ~trace
          cr;
        Cairo.translate ~x:0. ~y:proc_chart_height cr
      done;
      Cairo.identity_matrix cr;

      flush stderr;
      ()

    method on_scroll e =
      let state = GdkEvent.Scroll.state e in
      let modifiers = Gdk.Convert.modifier state in
      if List.mem `CONTROL modifiers
      then
        let x_px = self#clamp_x_to_chart (GdkEvent.Scroll.x e) in
        let x_relpos = (x_px -. self#chart_x_min ()) /. self#chart_width () in
        match GdkEvent.Scroll.direction e with
        | `UP ->
           self#zoom_in x_relpos
        | `DOWN ->
           self#zoom_out x_relpos
        | _ ->
           ()

    method on_configure e =
      let width = GdkEvent.Configure.width e - 10 in
      let height = int_of_float @@ self#height () in
      da#misc#set_size_request ~width ~height ();
      ()

    method on_click e =
      let x = GdkEvent.Button.x e in
      let y = GdkEvent.Button.y e in
      Format.eprintf "CLICK %f %f@." x y;
      ()

    initializer
    sw#set_hpolicy `AUTOMATIC;
    sw#set_vpolicy `AUTOMATIC;
    (* Expose and configure events *)
    let expose _ = self#on_expose (); false in
    ignore @@ da#event#connect#expose ~callback:expose;
    let configure e = self#on_configure e; false in
    ignore @@ da#event#connect#configure ~callback:configure;
    (* Scroll and click events *)
    let scroll e = self#on_scroll e; false in
    ignore @@ da#event#connect#scroll ~callback:scroll;
    let click e = self#on_click e; false in
    ignore @@ da#event#connect#button_press ~callback:click;
    ignore @@ da#event#add [`SCROLL; `BUTTON_PRESS];
    ()
  end
