open Utils
open Range

(* Activities and Events *)

type activity_kind = string

type event_kind = string

type signal_kind = string

type processor = int

(* Callbacks *)

type get_activities_callback =
  kind:activity_kind ->
  for_proc:processor ->
  between:span ->
  min_duration:float ->
  Range.span list

type get_events_callback =
  kind:event_kind ->
  for_proc:processor ->
  between:span ->
  float list

type get_signal_max_callback =
  kind:signal_kind ->
  float

type get_signal_samples_callback =
  kind:signal_kind ->
  between:span ->
  granularity:float ->
  float list

(* The Timeline control *)

(* Some terminological conventions:

      raw time = absolute time, used to communicate with the outside
   global time = time since the start of the global span
    local time = time since the start of the current span

   drawing pos = position in the drawing area
     chart pos = position in the chart inside the drawing area

   The various epochs (global, local, selection) are stored in raw time.
 *)

type t =
  {
    (* Global immutable parameters *)

    number_of_processors : int;

    (* Time parameters *)

    global_epoch : Range.span;

    mutable current_epoch : Range.span;

    mutable current_visible_chart_top : float;

    (* Selection parameters *)

    mutable current_selection : Range.span option;

    mutable selection_in_progress : bool;

    (* Visual parameters *)

    left_margin : int;

    color_background : Utils.rgb;

    color_selection : Utils.rgba;

    color_proc_active : Utils.rgba;

    scroll_zoom_factor : float;

    alpha_selection : float;

    scale_bar_number_of_increments : int;

    scale_bar_thickness : float;

    scale_bar_increments_height : float;

    scale_bar_increments_thickness : float;

    scale_bar_height : float;

    legend_right_margin : float;

    proc_chart_vertical_spacing : float;

    proc_chart_height : float;

    selection_bar_thickness : float;

    event_mark_thickness : float;

    event_mark_triangle_height : float;

    event_mark_triangle_base_width : float;

    activity_duration_factor : float;

    (* Activities and events *)

    get_activities : get_activities_callback;

    get_events : get_events_callback;

    get_signal_max : get_signal_max_callback;

    get_signal_samples : get_signal_samples_callback;

    mutable activities : (activity_kind * Utils.rgba) list;

    mutable events : (event_kind * Utils.rgba) list;

    mutable signals : (signal_kind * Utils.rgba) list;

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

let chart_right tl =
  width tl

let chart_top tl =
  tl.scale_bar_height

let chart_width tl =
  chart_right tl -. chart_left tl

let chart_height tl =
  height tl -. chart_top tl

let chart_dim tl =
  chart_width tl, chart_height tl

(* Conversion functions between time and graphical space *)

let current_epoch_range tl =
  Range.range tl.current_epoch

let time_per_pixel tl =
  current_epoch_range tl /. chart_width tl

let time_per_increment tl =
  current_epoch_range tl /. float tl.scale_bar_number_of_increments

let pixels_per_time tl =
  chart_width tl /. current_epoch_range tl

let pixels_per_increment tl =
  chart_width tl /. float tl.scale_bar_number_of_increments

let local_time_of_drawing_pos tl x =
  time_per_pixel tl *. (x -. chart_left tl)

let global_time_of_drawing_pos tl x =
  local_time_of_drawing_pos tl x +. tl.current_epoch.l -. tl.global_epoch.l

let raw_time_of_drawing_pos tl x =
  tl.current_epoch.l +. local_time_of_drawing_pos tl x

let drawing_pos_of_time tl t =
  let t = Range.truncate tl.current_epoch t in
  chart_left tl +. pixels_per_time tl *. (t -. tl.current_epoch.l)

let chart_pos_of_drawing_pos tl x =
  let x = Range.truncate { l = chart_left tl; u = chart_right tl; } x in
  x -. chart_left tl

(* Derived graphical parameters *)

let height_per_processor_bar tl =
  tl.proc_chart_height +. tl.proc_chart_vertical_spacing

let chart_preferred_height tl =
  height_per_processor_bar tl *. float tl.number_of_processors
  +. tl.scale_bar_height

let position_of_increment tl i =
  chart_left tl +. pixels_per_increment tl *. float i

let first_visible_processor tl =
  let top = int_of_float tl.current_visible_chart_top in
  let hpp = int_of_float @@ height_per_processor_bar tl in
  max 0 (top / hpp)

let number_of_visible_processors tl =
  let number_of_processor_bars =
    ceil (chart_height tl /. height_per_processor_bar tl)
  in
  min tl.number_of_processors (int_of_float number_of_processor_bars)

let visible_processors tl =
  let first = first_visible_processor tl in
  let last = first + number_of_visible_processors tl - 1 in
  first, last

let y_pos_of_processor_chart tl p =
  let p = p - first_visible_processor tl in
  if p < 0
  then
    Format.eprintf
      "WARNING: asking for position of processor %d which is not visible@."
      p;
  let p = max 0 p in
  chart_top tl +. float p *. height_per_processor_bar tl

let y_pos_of_processor_label tl p =
  y_pos_of_processor_chart tl p +. height_per_processor_bar tl /. 2.

(* Misc *)

let current_unit_scaling tl =
  find_good_unit_scaling @@ time_per_increment tl

let truncate_pos_to_chart tl x =
  Range.truncate { l = chart_left tl; u = chart_right tl; } x

(* Selection-related things *)

let selection_reset tl =
  tl.current_selection <- None

let selection_discrete tl t =
  tl.current_selection <- Some { l = t; u = t; }

let selection_right_side tl u =
  match tl.current_selection with
  | None ->
     selection_discrete tl u
  | Some { l; _ } ->
     tl.current_selection <- Some { l; u; }

(* Low-level Gtk+ stuff *)

let configure_scrollbars tl =
  (* Horizontal scrollbar *)
  tl.hsc#adjustment#set_lower tl.global_epoch.l;
  tl.hsc#adjustment#set_upper tl.global_epoch.u;
  tl.hsc#adjustment#set_value tl.current_epoch.l;
  tl.hsc#adjustment#set_page_size (current_epoch_range tl);
  tl.hsc#adjustment#set_step_increment (time_per_increment tl);
  (* Vertical scrollbar *)
  tl.vsc#adjustment#set_lower 0.;
  tl.vsc#adjustment#set_upper (chart_preferred_height tl);
  tl.vsc#adjustment#set_value tl.current_visible_chart_top;
  tl.vsc#adjustment#set_page_size (chart_height tl);
  ()

let redraw ?(scrollbars = false) tl =
  if scrollbars then configure_scrollbars tl;
  GtkBase.Widget.queue_draw tl.da#as_widget

(* High-level drawing functions *)

let draw_epoch ~y ~h tl cr s =
  let x0 = drawing_pos_of_time tl s.l in
  let x1 = drawing_pos_of_time tl s.u in
  let w = max (x1 -. x0) 1. in
  Cairo.rectangle cr ~x:x0 ~y ~w ~h;
  Cairo.fill cr

let draw_epoch_on_processor ~p tl cr s =
  let y = y_pos_of_processor_chart tl p in
  let h = tl.proc_chart_height in
  draw_epoch ~y ~h tl cr s

let draw_event_on_processor ~p tl cr t =
  let x = drawing_pos_of_time tl t in
  let y = y_pos_of_processor_chart tl p in
  let h = tl.proc_chart_height in

  let draw_triangle y dir =
    let x = Utils.middle x (x +. tl.event_mark_thickness) in
    let b = tl.event_mark_triangle_base_width /. 2. in
    Utils.triangle
      ~x0:(x -. b) ~y0:y
      ~x1:x ~y1:(y +. dir *. tl.event_mark_triangle_height)
      ~x2:(x +. b) ~y2:y
      cr
  in

  draw_triangle y 1.;
  draw_triangle (y +. h) (- 1.);
  Cairo.rectangle cr ~x ~y ~w:tl.event_mark_thickness ~h;
  Cairo.fill cr

(* Top-level functions *)

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
    let t = global_time_of_drawing_pos tl x *. t_scale in
    let label = Printf.sprintf "%.2f %s" t pref in
    (* Draw the small increment bar *)
    draw_increment ~label x;
  done;

  ()

let draw_legend tl cr =
  let p_min, p_max = visible_processors tl in
  let label_for_processor p = "P" ^ string_of_int p in
  let legend_label_width =
    let ext = Cairo.text_extents cr @@ label_for_processor p_max in
    ext.Cairo.x_advance
  in
  let x = chart_left tl -. legend_label_width -. tl.legend_right_margin in
  set_black cr;
  for p = p_min to p_max do
    let y = y_pos_of_processor_label tl p in
    Cairo.move_to cr ~x ~y;
    Cairo.Path.text cr (label_for_processor p);
    Cairo.stroke cr
  done;
  ()

let draw_processor_chart tl cr =
  let p_min, p_max = visible_processors tl in
  for p = p_min to p_max do
    (* Draw the default per-processor activity background. *)
    set_rgba cr tl.color_proc_active;
    draw_epoch_on_processor ~p tl cr tl.current_epoch;

    (* Draw activites. We do not draw the ones that are really small in order to
    try to lower query cost on huge files. *)

    let min_duration = time_per_pixel tl /. tl.activity_duration_factor in

    let draw_activities_of_kind (kind, color) =
      Utils.set_rgba cr color;
      tl.get_activities
        ~kind
        ~for_proc:p
        ~between:tl.current_epoch
        ~min_duration
      |> List.iter (draw_epoch_on_processor ~p tl cr)
    in

    List.iter draw_activities_of_kind tl.activities;

    let draw_events_of_kind (kind, color) =
      Utils.set_rgba cr color;
      tl.get_events
        ~kind
        ~for_proc:p
        ~between:tl.current_epoch
      |> List.iter (draw_event_on_processor ~p tl cr)
    in

    List.iter draw_events_of_kind tl.events;
  done;
  ()

let draw_selection tl cr =
  match tl.current_selection with
  | None ->
     ()
  | Some s ->
     set_rgba cr tl.color_selection;

     let x_l = drawing_pos_of_time tl s.l in
     let x_u = drawing_pos_of_time tl s.u in
     let y = chart_top tl in
     let h = chart_height tl in

     let draw_vertical_bar x =
       Cairo.rectangle
         cr
         ~x
         ~y
         ~w:tl.selection_bar_thickness
         ~h;
       Cairo.fill cr
     in

     let draw_intermediate_zone () =
       let r, g, b, _ = tl.color_selection in
       let w = x_u -. x_l in
       Cairo.set_source_rgba cr ~r ~g ~b ~a:tl.alpha_selection;
       Cairo.rectangle cr ~x:x_l ~y ~w ~h;
       Cairo.fill cr
     in

     draw_vertical_bar x_l;
     if x_l <> x_u then
       begin
         draw_vertical_bar x_u;
         draw_intermediate_zone ();
       end;
     ()
;;

let draw_timeline tl cr =
  draw_background tl cr;
  draw_scale_bar tl cr;
  draw_legend tl cr;
  draw_processor_chart tl cr;
  draw_selection tl cr;
  ()
;;

(* High-level actions *)

let change_current_epoch ~epoch tl =
  tl.current_epoch <- Range.clip ~within:tl.global_epoch epoch;
  redraw ~scrollbars:true tl

let zoom ~x ~factor tl =
  let xr = x /. chart_width tl in
  let latt = if xr < 0.1 then 0. else 1. in
  let ratt = if xr > 0.9 then 0. else 1. in
  let lchange = current_epoch_range tl *. factor *. latt *. xr in
  let rchange = current_epoch_range tl *. factor *. ratt *. (1. -. xr) in
  change_current_epoch
    ~epoch:{ l = tl.current_epoch.l +. lchange;
             u = tl.current_epoch.u -. rchange; }
    tl;
  ()

let zoom_in ~x tl =
  zoom ~x ~factor:tl.scroll_zoom_factor tl;
  ()

let zoom_out ~x tl =
  zoom ~x ~factor:(-. tl.scroll_zoom_factor) tl;
  ()

(* Gtk callbacks *)

let click tl pressed e =
  let x = GdkEvent.Button.x e in
  let t = raw_time_of_drawing_pos tl x in
  let button = GdkEvent.Button.button e in
  if button = 1 then
    begin
      if pressed then selection_discrete tl t;
      tl.selection_in_progress <- not tl.selection_in_progress;
      redraw tl;
    end;
  false

let move tl e =
  let x = GdkEvent.Motion.x e in
  let u = raw_time_of_drawing_pos tl x in
  if tl.selection_in_progress
  then
    begin
      selection_right_side tl u;
      redraw tl
    end;
  false

let expose tl _ =
  draw_timeline tl @@ Cairo_gtk.create tl.da#misc#window;
  false

let configure tl _ =
  configure_scrollbars tl;
  false

let scrollbar_value_changed tl () =
  (* Horizontal scrollbar *)
  let l = tl.hsc#adjustment#value in
  let u = l +. current_epoch_range tl in
  tl.current_epoch <- { l; u; };
  (* Vertical scrollbar *)
  tl.current_visible_chart_top <- tl.vsc#adjustment#value;
  redraw tl;
  ()

let mouse_wheel tl e =
  let state = GdkEvent.Scroll.state e in
  let modifiers = Gdk.Convert.modifier state in
  if List.mem `CONTROL modifiers
  then begin
      let x = chart_pos_of_drawing_pos tl @@ GdkEvent.Scroll.x e in
      match GdkEvent.Scroll.direction e with
      | `UP ->
         zoom_in ~x tl
      | `DOWN ->
         zoom_out ~x tl
      | _ ->
         ()
    end;
  false

(* Construction function *)

let make
      ~global_epoch
      ~number_of_processors
      ~get_activities
      ~get_events
      ~get_signal_max
      ~get_signal_samples
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

  let left_margin = 50 in
  let color_background = Utils.white_rgb in
  let color_selection = 0., 0.2, 1., 1. in
  let color_proc_active = 0.05, 0.05, 0.05, 0.08 in
  let alpha_selection = 0.3 in
  let scroll_zoom_factor = 0.1 in
  let scale_bar_number_of_increments = 10 in
  let scale_bar_thickness = 3. in
  let scale_bar_increments_height = 5. in
  let scale_bar_increments_thickness = 1.5 in
  let scale_bar_height = 30. in
  let legend_right_margin = 5. in
  let proc_chart_vertical_spacing = 2. in
  let proc_chart_height = 40. in
  let selection_bar_thickness = 3. in
  let event_mark_thickness = 2. in
  let event_mark_triangle_height = 15. in
  let event_mark_triangle_base_width = 10. in
  let activity_duration_factor = 100. in

  let da =
    GMisc.drawing_area
      ~packing:(tbl#attach ~top:0 ~left:0 ~expand:`BOTH)
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

      global_epoch;
      current_epoch = global_epoch;
      current_visible_chart_top = 0.;

      current_selection = None;
      selection_in_progress = false;

      left_margin;
      color_background;
      color_proc_active;
      color_selection;
      alpha_selection;
      scroll_zoom_factor;
      scale_bar_number_of_increments;
      scale_bar_thickness;
      scale_bar_increments_height;
      scale_bar_increments_thickness;
      scale_bar_height;
      legend_right_margin;
      proc_chart_vertical_spacing;
      proc_chart_height;
      selection_bar_thickness;
      event_mark_thickness;
      event_mark_triangle_height;
      event_mark_triangle_base_width;
      activity_duration_factor;

      get_activities;
      get_events;
      get_signal_max;
      get_signal_samples;

      activities = [];
      events = [];
      signals = [];

      tbl;
      da;
      vsc;
      hsc;
    }
  in
  ignore @@ da#event#connect#configure ~callback:(configure tl);
  ignore @@ da#event#connect#expose ~callback:(expose tl);
  ignore @@ da#event#connect#button_press ~callback:(click tl true);
  ignore @@ da#event#connect#button_release ~callback:(click tl false);
  ignore @@ da#event#connect#motion_notify ~callback:(move tl);
  ignore @@ da#event#connect#scroll ~callback:(mouse_wheel tl);
  ignore @@
    da#event#add
      [
        `SCROLL;
        `BUTTON_PRESS;
        `BUTTON_RELEASE;
        `POINTER_MOTION;
      ];
  ignore @@
    hsc#adjustment#connect#value_changed
      ~callback:(scrollbar_value_changed tl);
  ignore @@
    vsc#adjustment#connect#value_changed
      ~callback:(scrollbar_value_changed tl);
  tl

let remove_activity ~kind tl =
  tl.activities <- List.remove_assoc kind tl.activities;
  redraw tl

let add_activity ~kind ~color tl =
  remove_activity ~kind tl;     (* prevent duplicates *)
  tl.activities <- (kind, color) :: tl.activities;
  redraw tl

let remove_event ~kind tl =
  tl.events <- List.remove_assoc kind tl.events;
  redraw tl

let add_event ~kind ~color tl =
  remove_event ~kind tl;        (* prevent duplicates *)
  tl.events <- (kind, color) :: tl.events;
  redraw tl

let remove_signal ~kind tl =
  tl.signals <- List.remove_assoc kind tl.signals;
  redraw tl

let add_signal ~kind ~color tl =
  remove_signal ~kind tl;     (* prevent duplicates *)
  tl.signals <- (kind, color) :: tl.signals;
  redraw tl

let zoom_to_global tl =
  selection_reset tl;
  change_current_epoch ~epoch:tl.global_epoch tl

let zoom_to_selection tl =
  match tl.current_selection with
  | None ->
     ()
  | Some epoch ->
     selection_reset tl;
     change_current_epoch epoch tl
