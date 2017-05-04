open Cairo
open Utils

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

  set_black cr;

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

  let t_scale, pref = find_good_unit_scaling time_per_increment in

  Cairo.set_line_width cr 1.;
  for i = 0 to number_of_increments - 1 do
    let x = float i *. pixels_per_increment in
    (* Draw small label *)
    let x_time = float i *. time_per_increment +. cur_min_time -. min_time in
    let x_time = x_time *. t_scale in
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
  set_black cr;
  Cairo.move_to
    cr
    (-. label_extent.width -. 5.)
    (height /. 2. +. label_extent.height /. 2.);
  Cairo.Path.text cr label;
  Cairo.stroke cr;

  (* By default, a processor is considered active. *)
  set_rgba cr color_user;
  Cairo.rectangle cr 0. 0. width height;
  Cairo.fill cr;

  (* Draw GC period *)
  set_rgba cr color_gc;

  let total_time = cur_max_time -. cur_min_time in
  let x_ratio = width /. total_time in

  let draw_gc_period { Range.l = start; Range.u = stop; } =
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
      ~between:Range.{ l = cur_min_time; u = cur_max_time; }
      ~min_duration:0.000000001
      ~proc:processor
      trace
  in
  List.iter draw_gc_period gc_periods;

  ()
;;

let draw_selection
      ?(color = (0., 0.2, 1.))
      ?(alpha_interior = 0.3)
      ~bar_thickness
      ~height
      ~x_start
      ~x_stop
      cr =
  set_rgb cr color;

  let draw_vertical_bar x =
    Cairo.rectangle cr x 0. bar_thickness height;
    Cairo.fill cr
  in

  let draw_intermediate_zone () =
    let (r, g, b), a = color, alpha_interior in
    let width = x_stop -. x_start in
    Cairo.set_source_rgba cr ~r ~g ~b ~a:alpha_interior;
    Cairo.rectangle cr x_start 0. width height;
    Cairo.fill cr
  in

  draw_vertical_bar x_start;
  if x_stop <> x_start then
    begin
      draw_vertical_bar x_stop;
      draw_intermediate_zone ();
    end;

  ()
;;

let draw_events_of_kinds
      ~mark_thickness
      ~mark_radius
      ~height
      ~width
      ~cur_min_time
      ~cur_max_time
      ~processor
      ~kinds
      ~trace
      cr =
  let draw_events_of_kind (kind, color) =
    let events =
      Trace.events_between
        ~between:Range.{ l = cur_min_time; u = cur_max_time; }
        ~proc:processor
        ~kind
        trace
    in

    (* TODO factor out *)
    let x_ratio = width /. (cur_max_time -. cur_min_time) in
    let pos_of_time t = (t -. cur_min_time) *. x_ratio in

    set_rgba cr color;

    let draw_event_mark t =
      let x = pos_of_time t in
      let draw_bullet y =
        let x = middle x (x +. mark_radius) in
        Cairo.arc cr ~x ~y ~r:mark_radius ~a1:0. ~a2:pi2
      in

      Cairo.rectangle cr ~x ~y:0. ~w:mark_thickness ~h:height;
      draw_bullet 0.;
      draw_bullet height;
      Cairo.fill cr
    in

    List.iter draw_event_mark events
  in
  List.iter draw_events_of_kind kinds
;;

class timeline ~packing trace =
  let pcount = Trace.number_of_processors trace in

  let { Range.l = min_time; Range.u = max_time; } = Trace.epoch trace in

  let sw = GBin.scrolled_window ~packing:packing () in

  let da = GMisc.drawing_area ~packing:sw#add_with_viewport () in

  object (self)
    inherit GObj.widget da#as_widget

    (* Time-related fields *)

    val mutable cur_min_time = min_time

    val mutable cur_max_time = max_time

    val mutable cur_sel_start = min_time

    val mutable cur_sel_stop = min_time

    (* Trace event-related fields *)

    (* Which event kinds to print *)
    val mutable cur_selected_kinds = []

    (* State-machine fields *)

    val mutable selection_in_progress = false

    (* Appareance-related fields *)

    val scroll_zoom_factor = 0.05

    val scale_bar_number_of_increments = 10

    val scale_bar_thickness = 1.

    val scale_bar_increments_height = 4.

    val scale_bar_height = 20.

    val proc_chart_vertical_spacing = 2.

    val proc_chart_height = 30.

    val event_bar_thickness = 1.

    val event_mark_thickness = 2.

    val event_mark_radius = 2.

    (* Methods computing appearance-related things *)

    method chart_height () =
      float pcount *. (proc_chart_height +. proc_chart_vertical_spacing)

    method height () =
      scale_bar_height +. self#chart_height ()

    method width () =
      let al = sw#misc#allocation in
      float (al.Gtk.width - 10)

    method chart_width () =
      self#width () *. 0.9

    method chart_x_min () =
      (self#width () -. self#chart_width ()) *. 0.9

    method chart_x_max () =
      self#chart_x_min () +. self#chart_width ()

    (* Methods computing time-related things *)

    method cur_time_span () =
      cur_max_time -. cur_min_time

    method truncate_x_to_chart x =
      min (max x (self#chart_x_min ())) (self#chart_x_max ())

    method truncate_time_to_cur_time_span t =
      min (max t cur_min_time) cur_max_time

    method time_to_chart_pos x =
      let x = x -. cur_min_time in
      (self#chart_width () /. self#cur_time_span ()) *. x

    method absolute_pos_to_time x =
      let x = x -. self#chart_x_min () in
      cur_min_time +. (self#cur_time_span () /. self#chart_width ()) *. x

    (* Methods for low-level access *)

    method repaint () =
      GtkBase.Widget.queue_draw da#as_widget

    (* Methods updating zoom level *)

    method zoom_to new_min_time new_max_time =
      cur_min_time <- max min_time new_min_time;
      cur_max_time <- min max_time new_max_time;
      (* Truncate selection *)
      self#set_sel_start cur_sel_start;
      self#set_sel_stop cur_sel_stop;
      (* Ask GTK to send an expose event *)
      self#repaint ()

    method zoom_adjust x_prop zoom_factor =
      let adjustment = zoom_factor *. self#cur_time_span () in

      let start_adjustment =
        if x_prop <= 0.2 then 0. else adjustment *. x_prop
      in
      let stop_adjustment =
        if x_prop >= 0.8 then 0. else adjustment *. (1. -. x_prop)
      in

      let new_min_time = cur_min_time +. start_adjustment in
      let new_max_time = cur_max_time -. stop_adjustment in
      self#zoom_to new_min_time new_max_time

    method zoom_in x_prop =
      self#zoom_adjust x_prop scroll_zoom_factor

    method zoom_out x_prop =
      self#zoom_adjust x_prop (-. scroll_zoom_factor)

    method zoom_to_default () =
      self#zoom_to min_time max_time

    method zoom_to_selection () =
      if cur_sel_start <> cur_sel_stop then
        begin
          let left = min cur_sel_start cur_sel_stop in
          let right = max cur_sel_start cur_sel_stop in
          self#zoom_to left right;
          self#reset_selection ();
        end;
      ()

    (* Methods updating selection *)

    method set_sel_start start =
      cur_sel_start <- self#truncate_time_to_cur_time_span start

    method set_sel_stop stop =
      cur_sel_stop <- self#truncate_time_to_cur_time_span stop

    method reset_selection () =
      cur_sel_start <- cur_min_time;
      cur_sel_stop <- cur_min_time;
      self#repaint ()

    (* Methods updating selection *)

    method toggle_kind event_kind color =
      if List.mem_assoc event_kind cur_selected_kinds
      then
        cur_selected_kinds <- List.remove_assoc event_kind cur_selected_kinds
      else
        cur_selected_kinds <- (event_kind, color) :: cur_selected_kinds;
      self#repaint ();
      ()

    (* Event handlers *)

    method on_expose () =
      let cr = Cairo_gtk.create da#misc#window in
      let chart_width = self#chart_width () in
      let chart_offset = self#chart_x_min () in

      let chart_matrix () =
        Cairo.identity_matrix cr;
        Cairo.translate cr chart_offset scale_bar_height;
      in

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

      chart_matrix ();

      (* Draw each processor's chart *)
      for p = 0 to pcount - 1 do
        Cairo.translate ~x:0. ~y:proc_chart_vertical_spacing cr;
        draw_processor_chart
          ~width:chart_width
          ~height:proc_chart_height
          ~cur_min_time
          ~cur_max_time
          ~processor:p
          ~trace
          cr;
        draw_events_of_kinds
          ~mark_thickness:event_mark_thickness
          ~mark_radius:event_mark_radius
          ~width:chart_width
          ~height:proc_chart_height
          ~cur_min_time
          ~cur_max_time
          ~processor:p
          ~kinds:cur_selected_kinds
          ~trace
          cr;
        Cairo.translate ~x:0. ~y:proc_chart_height cr
      done;
      Cairo.identity_matrix cr;

      chart_matrix ();

      draw_selection
        ~bar_thickness:event_bar_thickness
        ~height:(self#chart_height ())
        ~x_start:(self#time_to_chart_pos cur_sel_start)
        ~x_stop:(self#time_to_chart_pos cur_sel_stop)
        cr;

      flush stderr;
      ()

    method on_scroll e =
      let state = GdkEvent.Scroll.state e in
      let modifiers = Gdk.Convert.modifier state in
      if List.mem `CONTROL modifiers
      then
        let x_px = self#truncate_x_to_chart (GdkEvent.Scroll.x e) in
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

    method on_click pressed e =
      let x = GdkEvent.Button.x e in
      let x_time = self#absolute_pos_to_time x in
      let button = GdkEvent.Button.button e in
      if button = 1 then
        begin
          if pressed then
            begin
              self#set_sel_start x_time;
              self#set_sel_stop x_time
            end;
          selection_in_progress <- not selection_in_progress;
          self#repaint ()
        end;
      ()

    method on_mouse_motion e =
      let x = GdkEvent.Motion.x e in
      let x_time = self#absolute_pos_to_time x in
      if selection_in_progress
      then
        begin
          self#set_sel_stop x_time;
          self#repaint ()
        end;
      ()

    (* Initialization code *)

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
    let click pressed e = self#on_click pressed e; false in
    ignore @@ da#event#connect#button_press ~callback:(click true);
    ignore @@ da#event#connect#button_release ~callback:(click false);
    let motion e = self#on_mouse_motion e; false in
    ignore @@ da#event#connect#motion_notify ~callback:motion;
    ignore @@
      da#event#add
        [
          `SCROLL;
          `BUTTON_PRESS;
          `BUTTON_RELEASE;
          `POINTER_MOTION;
        ];
    ()
  end
