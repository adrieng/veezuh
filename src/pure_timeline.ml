open Time
open Utils

(* Activities and Events *)

type activity_kind = string

type event_kind = string

type processor = int

(* Selection *)

let default_selection s =
  { l = s.l; u = s.l; }

(* Callbacks *)

type get_activities_callback =
  kind:activity_kind ->
  for_proc:processor ->
  between:Time.span ->
  min_duration:Time.time ->
  Time.span list

type get_events_callback =
  kind:event_kind ->
  for_proc:processor ->
  between:Time.span ->
  time list

(* The Timeline control *)

type t =
  {
    (* Global immutable parameters *)

    number_of_processors : int;

    (* Time parameters *)

    global_span : Time.span;

    mutable current_span : Time.span;

    (* Selection parameters *)

    mutable current_selection : Time.span;

    mutable selection_in_progress : bool;

    (* Visual parameters *)

    left_margin : int;

    color_background : Utils.rgb;

    color_proc_active : Utils.rgba;

    scroll_zoom_factor : float;

    scale_bar_number_of_increments : int;

    scale_bar_thickness : float;

    scale_bar_increments_height : float;

    scale_bar_increments_thickness : float;

    scale_bar_height : float;

    proc_chart_vertical_spacing : float;

    proc_chart_height : float;

    selection_bar_thickness : float;

    event_mark_thickness : float;

    event_mark_radius : float;

    activity_duration_factor : float;

    (* Activities and events *)

    get_activities : get_activities_callback;

    get_events : get_events_callback;

    mutable activities : (activity_kind * Utils.rgba) list;

    mutable events : (event_kind * Utils.rgba) list;

    (* Gtk controls *)

    tbl : GPack.table;

    da : GMisc.drawing_area;

    vsc : GRange.range;

    hsc : GRange.range;
  }

(* Functions computing layout positions *)

let width tl =
  float tl.da#misc#allocation.Gtk.width

let height tl =
  float tl.da#misc#allocation.Gtk.height

let chart_left tl =
  float tl.left_margin

let chart_top tl =
  tl.scale_bar_height

let chart_width tl =
  width tl -. chart_left tl

let chart_height tl =
  height tl

let chart_dim tl =
  chart_width tl, chart_height tl

(* Conversion functions between time and graphical space *)

let time_per_pixel tl =
  Time.range tl.current_span /. chart_width tl

let time_per_increment tl =
  Time.range tl.current_span /. float tl.scale_bar_number_of_increments

let pixels_per_time tl =
  chart_width tl /. Time.range tl.current_span

let pixels_per_increment tl =
  chart_width tl /. float tl.scale_bar_number_of_increments

let relative_time_of_relative_pos tl x =
  time_per_pixel tl *. x

let position_of_increment tl i =
  chart_left tl +. pixels_per_increment tl *. float i

let current_unit_scaling tl =
  find_good_unit_scaling @@ time_per_increment tl

let height_per_processor_bar tl =
  tl.proc_chart_height +. tl.proc_chart_vertical_spacing

let y_pos_of_processor_chart tl p =
  chart_top tl +. float p *. height_per_processor_bar tl

let y_pos_of_processor_label tl p =
  y_pos_of_processor_chart tl p +. height_per_processor_bar tl /. 2.

let absolute_pos_of_time tl t =
  let t = Time.truncate tl.current_span t in
  chart_left tl +. pixels_per_time tl *. (t -. tl.current_span.l)

(* High-level drawing functions *)

let draw_span ~y ~h tl cr s =
  let x0 = absolute_pos_of_time tl s.l in
  let x1 = absolute_pos_of_time tl s.u in
  let w = max (x1 -. x0) 1. in
  Cairo.rectangle cr ~x:x0 ~y ~w ~h;
  Cairo.fill cr

let draw_span_on_processor ~p tl cr s =
  let y = y_pos_of_processor_chart tl p in
  let h = tl.proc_chart_height in
  draw_span ~y ~h tl cr s

let draw_event_on_processor ~p tl cr t =
  let x = absolute_pos_of_time tl t in
  let y = y_pos_of_processor_chart tl p in
  let h = tl.proc_chart_height in

  let draw_bullet y =
    let x = middle x (x +. tl.event_mark_radius) in
    Cairo.arc cr ~x ~y ~r:tl.event_mark_radius ~a1:0. ~a2:pi2
  in

  draw_bullet y;
  draw_bullet (y +. h);
  Cairo.rectangle cr ~x ~y ~w:tl.event_mark_thickness ~h;
  Cairo.fill cr

(* Top-level functions *)

let redraw tl =
  GtkBase.Widget.queue_draw tl.da#as_widget

let draw_background tl cr =
  (* Fill background color *)

  set_rgb cr tl.color_background;
  Cairo.rectangle
    cr
    ~x:(chart_left tl)
    ~y:(chart_top tl)
    ~w:(chart_width tl)
    ~h:(chart_height tl);
  Cairo.fill cr;

  (* Draw vertical bars. *)
  for i = 0 to tl.scale_bar_number_of_increments - 1 do
    let x = position_of_increment tl i in
    Cairo.set_source_rgba cr 0. 0. 0. 0.4;
    Cairo.rectangle cr ~x ~y:(chart_top tl) ~w:1. ~h:(chart_height tl);
    Cairo.fill cr
  done;

  ()

let draw_scale_bar
      tl
      cr
  =
  let w, h = chart_dim tl in

  set_black cr;

  (* Draw the main axis. *)
  Cairo.rectangle
    cr
    ~x:(chart_left tl)
    ~y:(tl.scale_bar_height -. tl.scale_bar_thickness)
    ~w
    ~h:tl.scale_bar_thickness;
  Cairo.fill cr;

  (* Draw the increments *)

  let draw_increment ?(label = "") x =
    let y = tl.scale_bar_height -. tl.scale_bar_thickness in
    (* Draw the increment bar *)
    Cairo.rectangle
      cr
      ~x
      ~y
      ~w:tl.scale_bar_increments_thickness
      ~h:(-. tl.scale_bar_increments_height);
    Cairo.fill cr;

    (* Draw increment label, if any *)
    Cairo.move_to
      cr
      ~x:x
      ~y:(y -. tl.scale_bar_increments_height -. 4.);
    Cairo.Path.text cr label;
    Cairo.stroke cr;
  in

  let t_scale, pref = current_unit_scaling tl in

  Cairo.set_line_width cr 1.;
  for i = 0 to tl.scale_bar_number_of_increments - 1 do
    let x = position_of_increment tl i in
    let t = relative_time_of_relative_pos tl x *. t_scale in
    let label = Printf.sprintf "%.2f %s" t pref in
    (* Draw the small increment bar *)
    draw_increment ~label x;
  done;

  ()

let draw_legend tl cr =
  set_black cr;
  for p = 0 to tl.number_of_processors - 1 do
    let y = y_pos_of_processor_label tl p in
    Cairo.move_to cr ~x:10. ~y;
    Cairo.Path.text cr ("Processor " ^ string_of_int p);
    Cairo.stroke cr
  done;
  ()

let draw_processor_chart tl cr =
  for p = 0 to tl.number_of_processors - 1 do
    (* Draw the default per-processor activity background. *)
    set_rgba cr tl.color_proc_active;
    draw_span_on_processor ~p tl cr tl.current_span;

    (* Draw activites. We do not draw the ones that are really small in order to
    try to lower query cost on huge files. *)

    let min_duration = time_per_pixel tl /. tl.activity_duration_factor in

    let draw_activities_of_kind (kind, color) =
      Utils.set_rgba cr color;
      tl.get_activities
        ~kind
        ~for_proc:p
        ~between:tl.current_span
        ~min_duration
      |> List.iter (draw_span_on_processor ~p tl cr)
    in

    List.iter draw_activities_of_kind tl.activities;

    let draw_events_of_kind (kind, color) =
      Utils.set_rgba cr color;
      tl.get_events
        ~kind
        ~for_proc:p
        ~between:tl.current_span
      |> List.iter (draw_event_on_processor ~p tl cr)
    in

    List.iter draw_events_of_kind tl.events;
  done;
  ()

(* Gtk callbacks *)

let expose tl _ =
  let cr = Cairo_gtk.create tl.da#misc#window in

  draw_background tl cr;

  draw_scale_bar tl cr;

  draw_legend tl cr;

  draw_processor_chart tl cr;

  false

(* Construction function *)

let make
      ~global_span
      ~number_of_processors
      ~get_activities
      ~get_events
      ~packing
      () =
  let tbl =
    GPack.table
      ~columns:2
      ~rows:2
      ~row_spacings:0
      ~col_spacings:0
      ~border_width:0
      ~homogeneous:false
      ~packing
      ()
  in

  let left_margin = 100 in
  let color_background = Utils.white_rgb in
  let color_proc_active = 0.05, 0.05, 0.05, 0.08 in
  let scroll_zoom_factor = 0.05 in
  let scale_bar_number_of_increments = 10 in
  let scale_bar_thickness = 3. in
  let scale_bar_increments_height = 5. in
  let scale_bar_increments_thickness = 1.5 in
  let scale_bar_height = 30. in
  let proc_chart_vertical_spacing = 2. in
  let proc_chart_height = 40. in
  let selection_bar_thickness = 1. in
  let event_mark_thickness = 2. in
  let event_mark_radius = 2.5 in
  let activity_duration_factor = 100. in

  let preferred_height =
    int_of_float scale_bar_height
    + int_of_float proc_chart_height * number_of_processors
  in

  let da =
    GMisc.drawing_area
      ~height:preferred_height
      ~packing:(tbl#attach ~top:0 ~left:0 ~expand:`X)
      ()
  in
  let vsc =
    GRange.scrollbar
      `VERTICAL
      ~packing:(tbl#attach ~top:0 ~left:1 ~shrink:`BOTH)
      ()
  in
  let hsc =
    GRange.scrollbar
      `HORIZONTAL
      ~packing:(tbl#attach ~top:1 ~left:0 ~shrink:`BOTH)
      ()
  in

  let tl =
    {
      number_of_processors;

      global_span;
      current_span = global_span;

      current_selection = default_selection global_span;
      selection_in_progress = false;

      left_margin;
      color_background;
      color_proc_active;
      scroll_zoom_factor;
      scale_bar_number_of_increments;
      scale_bar_thickness;
      scale_bar_increments_height;
      scale_bar_increments_thickness;
      scale_bar_height;
      proc_chart_vertical_spacing;
      proc_chart_height;
      selection_bar_thickness;
      event_mark_thickness;
      event_mark_radius;
      activity_duration_factor;

      get_activities;
      get_events;
      activities = [];
      events = [];

      tbl;
      da;
      vsc;
      hsc;
    }
  in
  ignore @@ da#event#connect#expose ~callback:(expose tl);
  tl

let add_activity ~kind ~color tl =
  tl.activities <- (kind, color) :: tl.activities;
  redraw tl

let add_event ~kind ~color tl =
  tl.events <- (kind, color) :: tl.events;
  redraw tl
