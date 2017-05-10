open Utils
open Range

(* Keys *)

type signal_samples_callback =
  between:Range.span ->
  granularity:float ->
  (float * float) list

type activity_callback =
  between:Range.span ->
  min_duration:Range.time ->
  Range.span list

type event_callback =
  between:Range.span ->
  Range.time list

type key_kind =
  | Signal of signal_kind
  | Activity of activity_kind
  | Event of event_kind

and signal_kind =
  {
    max : float;
    samples : signal_samples_callback;
    alpha_mult : float;
  }

and activity_kind =
  {
    activities : activity_callback;
  }

and event_kind =
  {
    events : event_callback;
  }

type key =
  {
    name : string;
    kind : key_kind;
    color : Utils.rgba;
    mutable visible : bool;
  }

type key_name = string

let make_key ~name ~kind ~color ~visible =
  { name; kind; color; visible; }

let get_key_name { name; _ } =
  name

let get_key_kind { kind; _ } =
  kind

let get_key_color { color; _ } =
  color

let get_key_visible { visible; _ } =
  visible

let set_key_visible k ~visible =
  k.visible <- visible

(* Rows *)

type row_name = string

type row =
  {
    name : string;
    rank : int;
    height : int;
    background : Utils.rgba;
    mutable visible : bool;
    mutable keys : key list;
  }

let get_row_name { name; _ } =
  name

let get_row_height { height; _ } =
  height

let get_row_background { background; _ } =
  background

let get_row_visible { visible; _ } =
  visible

let set_row_visible r ~visible =
  r.visible <- visible

let make_row ~name ~rank ~height ~background ~visible =
  { name; rank; height; background; visible; keys = []; }

let row_add_key r k =
  r.keys <- k :: r.keys

let row_find_key r name =
  List.find (fun (k : key) -> k.name = name) r.keys

let row_iter_keys f r =
  List.iter f r.keys

let row_fold_keys f acc r =
  List.fold_left f acc r.keys

(* The Timeline control *)

(* Some terminological conventions:

      raw time = absolute time, used to communicate with the outside
   global time = time since the start of the global span
    local time = time since the start of the current span

   drawing x-pos = x-position in the drawing area
     chart x-pos = x-position in the chart inside the drawing area

   absolute y-pos = y-position in the total area, including hidden parts
   relative y-pos = y-position in the visible area

   The various epochs (global, local, selection) are stored in raw time.
 *)

type t =
  {
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

    scroll_zoom_factor : float;

    alpha_selection : float;

    scale_bar_number_of_increments : int;

    scale_bar_thickness : float;

    scale_bar_increments_height : float;

    scale_bar_increments_thickness : float;

    scale_bar_height : int;

    legend_right_margin : float;

    chart_vertical_spacing : int;

    selection_bar_thickness : float;

    event_mark_thickness : float;

    event_mark_triangle_height : float;

    event_mark_triangle_base_width : float;

    activity_duration_factor : float;

    (* Activities and events *)

    mutable rows : row list;

    (* Gtk controls *)

    tbl : GPack.table;

    da : GMisc.drawing_area;

    vsc : GRange.range;

    hsc : GRange.range;

    (* Offscreen drawing buffer *)

    mutable img : Cairo.Surface.t;

    mutable img_dirty : bool;
  }

(* Low-level functions *)

let number_of_rows tl =
  List.length tl.rows

(* Functions computing layout positions *)

let width tl =
  tl.da#misc#allocation.Gtk.width

let height tl =
  tl.da#misc#allocation.Gtk.height

let chart_left tl =
  tl.left_margin

let chart_right tl =
  width tl

let chart_top tl =
  tl.scale_bar_height

let chart_width tl =
  chart_right tl - chart_left tl

let chart_height tl =
  height tl - chart_top tl

let chart_dim tl =
  chart_width tl, chart_height tl

let chart_left_f tl =
  float @@ chart_left tl

let chart_right_f tl =
  float @@ chart_right tl

let chart_top_f tl =
  float @@ chart_top tl

let chart_width_f tl =
  float @@ chart_width tl

let chart_height_f tl =
  float @@ chart_height tl

let chart_dim_f tl =
  let w, h = chart_dim tl in
  float w, float h

(* conversion functions between time and graphical space *)

let current_epoch_range tl =
  Range.range tl.current_epoch

let time_per_pixel tl =
  current_epoch_range tl /. chart_width_f tl

let time_per_increment tl =
  current_epoch_range tl /. float tl.scale_bar_number_of_increments

let pixels_per_time tl =
  chart_width_f tl /. current_epoch_range tl

let pixels_per_increment tl =
  chart_width_f tl /. float tl.scale_bar_number_of_increments

let local_time_of_drawing_pos tl x =
  time_per_pixel tl *. (x -. chart_left_f tl)

let global_time_of_drawing_pos tl x =
  local_time_of_drawing_pos tl x +. tl.current_epoch.l -. tl.global_epoch.l

let raw_time_of_drawing_pos tl x =
  tl.current_epoch.l +. local_time_of_drawing_pos tl x

let drawing_pos_of_time tl t =
  let t = Range.truncate tl.current_epoch t in
  chart_left_f tl +. pixels_per_time tl *. (t -. tl.current_epoch.l)

let chart_pos_of_drawing_pos tl x =
  let x =
    Range.truncate
      { l = chart_left_f tl; u = chart_right_f tl; }
      x
  in
  x -. chart_left_f tl

(* Derived graphical parameters *)

let total_chart_height tl =
  number_of_rows tl * tl.chart_vertical_spacing
  + List.fold_left (fun acc r -> acc + r.height) 0 tl.rows

let total_chart_height_f tl =
  float @@ total_chart_height tl

let position_of_increment tl i =
  float (chart_left tl) +. pixels_per_increment tl *. float i

let iter_rows_between f tl ~ymin ~ymax =
  let rec iter y rows =
    match rows with
    | [] ->
       ()
    | r :: rows ->
       if r.visible
       then
         begin
           if y + r.height >= ymin then
             begin
               let yreal = if y < ymin then ymin else y in
               let h = if y + r.height >= ymax then ymax - y else r.height in
               f ~ymin:y ~y:yreal ~h r
             end;
           let y = y + r.height + tl.chart_vertical_spacing in
           if y < ymax then iter y rows
         end
       else
         iter y rows
  in
  iter (chart_top tl) tl.rows

let iter_visible_rows f tl =
  let ymin = int_of_float tl.current_visible_chart_top in
  let ymax = ymin + chart_height tl in
  iter_rows_between f tl ~ymin ~ymax

(* Misc *)

let current_unit_scaling tl =
  find_good_unit_scaling @@ time_per_increment tl

let truncate_pos_to_chart tl x =
  Range.truncate
    { l = float (chart_left tl); u = float (chart_right tl); }
    x

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
  tl.vsc#adjustment#set_upper (total_chart_height_f tl);
  tl.vsc#adjustment#set_value tl.current_visible_chart_top;
  tl.vsc#adjustment#set_page_size (chart_height_f tl);
  ()

let redraw ?(dirty = false) ?(scrollbars = false) tl =
  tl.img_dirty <- tl.img_dirty || dirty;
  if scrollbars then configure_scrollbars tl;
  GtkBase.Widget.queue_draw tl.da#as_widget

(* High-level drawing functions *)

let draw_epoch ~y ~h tl cr s =
  let x0 = drawing_pos_of_time tl s.l in
  let x1 = drawing_pos_of_time tl s.u in
  let w = max (x1 -. x0) 1. in
  if w >= 1. then
    begin
      Cairo.rectangle cr ~x:x0 ~y ~w ~h;
      Cairo.fill cr
    end;
  ()

let draw_event ~y ~h tl cr t =
  let x = drawing_pos_of_time tl t in

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
    ~x:(float @@ chart_left tl)
    ~y:(float @@ chart_top tl)
    ~w:(float @@ chart_width tl)
    ~h:(float @@ chart_height tl);
  Cairo.fill cr;

  (* Draw vertical bars. *)
  for i = 0 to tl.scale_bar_number_of_increments - 1 do
    let x = position_of_increment tl i in
    Cairo.set_source_rgba cr 0. 0. 0. 0.4;
    Cairo.rectangle cr
      ~x
      ~y:(float @@ chart_top tl)
      ~w:1.
      ~h:(float @@ chart_height tl);
    Cairo.fill cr
  done;

  ()

let draw_scale_bar
      tl
      cr
  =
  let w, h = chart_dim_f tl in

  set_black cr;

  (* Draw the main axis. *)
  Cairo.rectangle
    cr
    ~x:(float @@ chart_left tl)
    ~y:(float tl.scale_bar_height -. tl.scale_bar_thickness)
    ~w
    ~h:tl.scale_bar_thickness;
  Cairo.fill cr;

  (* Draw the increments *)

  let draw_increment ?(label = "") x =
    let y = float tl.scale_bar_height -. tl.scale_bar_thickness in
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

let draw_key ~ymin ~y ~h tl cr (key : key) =
  if key.visible then
    begin
      let between = tl.current_epoch in
      set_rgba cr key.color;
      match key.kind with
      | Signal ksignal ->
         let granularity = time_per_pixel tl in
         let vmax = ksignal.max in
         let samples = ksignal.samples ~between ~granularity in

         let ymax = y +. h in
         let yr = { l = y +. 1.; u = ymax -. 1.; } in

         Cairo.set_line_join cr Cairo.JOIN_ROUND;
         Cairo.set_line_width cr 2.;

         (* Start the mask at the bottom-right of the proc chart *)

         let lx = ref (chart_left_f tl) in
         let ly = ref ymax in
         Cairo.move_to cr ~x:!lx ~y:!ly;

         (* Add each sample to the mask *)
         let draw_sample (t, v) =
           let x = drawing_pos_of_time tl t in
           let y = ymax -. v /. vmax *. h in
           let y = Range.truncate yr y in
           if ceil y <> ceil !ly
           then
             begin
               Cairo.line_to cr ~x:x ~y:!ly;
               Cairo.line_to cr ~x ~y;
               lx := x;
               ly := y;
             end;
         in
         List.iter draw_sample samples;

         (* Close the mask *)
         Cairo.line_to cr ~x:(chart_right_f tl) ~y:!ly;
         Cairo.stroke_preserve cr;

         (* Draw the underline *)
         Utils.(set_rgba cr @@ mult_alpha key.color ksignal.alpha_mult);
         Cairo.line_to cr ~x:(chart_right_f tl) ~y:ymax;
         Cairo.line_to cr ~x:(chart_left_f tl) ~y:ymax;
         Cairo.Path.close cr;
         Cairo.fill cr

      | Activity kactivity ->
         let min_duration = time_per_pixel tl in
         let activities = kactivity.activities ~between ~min_duration in
         List.iter (draw_epoch ~y ~h tl cr) activities

      | Event kevent ->
         let events = kevent.events ~between in
         List.iter (draw_event ~y ~h tl cr) events
    end

let draw_row tl cr ~ymin ~y ~h row =
  let y = float y in
  let h = float h in

  (* Draw legend *)
  Utils.set_rgba cr Utils.black;
  let draw_label_right_aligned label =
    let y = y +. h /. 2. in
    let l = Cairo.((text_extents cr row.name).x_advance) in
    let x = chart_left_f tl -. tl.legend_right_margin -. l in
    Cairo.move_to cr ~x ~y;
    Cairo.Path.text cr label;
    Cairo.stroke cr
  in
  draw_label_right_aligned row.name;

  (* Draw background *)
  Utils.set_rgba cr row.background;
  draw_epoch ~y ~h tl cr tl.current_epoch;

  (* Draw keys *)
  List.iter (draw_key ~ymin ~y ~h tl cr) row.keys;

  ()

(* let draw_processors_chart tl cr = *)
(*   let p_min, p_max = visible_processors tl in *)
(*   for p = p_min to p_max do *)
(*     (\* Draw the default per-processor activity background. *\) *)
(*     set_rgba cr tl.color_proc_active; *)
(*     draw_epoch_on_processor ~p tl cr tl.current_epoch; *)

(*     (\* Draw activites. We do not draw the ones that are really small in order to *)
(*     try to lower query cost on huge files. *\) *)

(*     let min_duration = time_per_pixel tl /. tl.activity_duration_factor in *)

(*     let draw_activities_of_kind (kind, color) = *)
(*       Utils.set_rgba cr color; *)
(*       tl.get_activities *)
(*         ~kind *)
(*         ~for_proc:p *)
(*         ~between:tl.current_epoch *)
(*         ~min_duration *)
(*       |> List.iter (draw_epoch_on_processor ~p tl cr) *)
(*     in *)

(*     List.iter draw_activities_of_kind tl.activities; *)

(*     let draw_events_of_kind (kind, color) = *)
(*       Utils.set_rgba cr color; *)
(*       tl.get_events *)
(*         ~kind *)
(*         ~for_proc:p *)
(*         ~between:tl.current_epoch *)
(*       |> List.iter (draw_event_on_processor ~p tl cr) *)
(*     in *)

(*     List.iter draw_events_of_kind tl.events; *)
(*   done; *)
(*   () *)

let draw_selection tl cr =
  match tl.current_selection with
  | None ->
     ()
  | Some s ->
     set_rgba cr tl.color_selection;

     let x_l = drawing_pos_of_time tl s.l in
     let x_u = drawing_pos_of_time tl s.u in
     let y = chart_top_f tl in
     let h = chart_height_f tl in

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

let draw_buffered tl cr =
  if tl.img_dirty
  then
    begin
      tl.img_dirty <- false;
      tl.img <-
        Cairo.Surface.create_similar
          (Cairo.get_target cr)
          Cairo.COLOR_ALPHA
          ~width:(width tl)
          ~height:(height tl);
      let cr_os = Cairo.create tl.img in
      draw_background tl cr_os;
      draw_scale_bar tl cr_os;
      iter_visible_rows (draw_row tl cr_os) tl
    end;
  Cairo.set_source_surface cr tl.img ~x:0. ~y:0.;
  Cairo.paint cr;
  ()

let draw_timeline tl cr =
  draw_buffered tl cr;
  draw_selection tl cr;
  ()
;;

(* High-level actions *)

let change_current_epoch ~epoch tl =
  tl.current_epoch <- Range.clip ~within:tl.global_epoch epoch;
  redraw ~dirty:true ~scrollbars:true tl

let zoom ~x ~factor tl =
  let xr = x /. chart_width_f tl in
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
  let cr = Cairo_gtk.create tl.da#misc#window in
  draw_timeline tl cr;
  false

let configure tl _ =
  configure_scrollbars tl;
  tl.img_dirty <- true;
  false

let scrollbar_value_changed tl () =
  (* Horizontal scrollbar *)
  let l = tl.hsc#adjustment#value in
  let u = l +. current_epoch_range tl in
  tl.current_epoch <- { l; u; };
  (* Vertical scrollbar *)
  tl.current_visible_chart_top <- tl.vsc#adjustment#value;
  redraw ~dirty:true tl;
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
  let alpha_selection = 0.3 in
  let scroll_zoom_factor = 0.1 in
  let scale_bar_number_of_increments = 10 in
  let scale_bar_thickness = 3. in
  let scale_bar_increments_height = 5. in
  let scale_bar_increments_thickness = 1.5 in
  let scale_bar_height = 30 in
  let legend_right_margin = 5. in
  let chart_vertical_spacing = 2 in
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
      global_epoch;
      current_epoch = global_epoch;
      current_visible_chart_top = 0.;

      current_selection = None;
      selection_in_progress = false;

      left_margin;
      color_background;
      color_selection;
      alpha_selection;
      scroll_zoom_factor;
      scale_bar_number_of_increments;
      scale_bar_thickness;
      scale_bar_increments_height;
      scale_bar_increments_thickness;
      scale_bar_height;
      legend_right_margin;
      chart_vertical_spacing;
      selection_bar_thickness;
      event_mark_thickness;
      event_mark_triangle_height;
      event_mark_triangle_base_width;
      activity_duration_factor;

      rows = [];

      tbl;
      da;
      vsc;
      hsc;

      img = Utils.offscreen_surface_for_drawing_area da;
      img_dirty = true;
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

let refresh tl =
  redraw ~dirty:true ~scrollbars:true tl

let add_row tl r =
  let new_rows = r :: tl.rows in
  (* TODO very inefficient! *)
  tl.rows <- List.sort (fun r1 r2 -> compare r1.rank r2.rank) new_rows

let find_row tl name =
  List.find (fun r -> r.name = name) tl.rows

let iter_rows f tl =
  List.iter f tl.rows

let fold_rows f acc tl =
  List.fold_left f acc tl.rows

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
