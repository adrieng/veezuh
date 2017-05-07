(* Main graphical interface *)

open GMain

let width = 800

let height = 600

type key_type =
  | Signal
  | Activity
  | Event

type key =
  {
    ty : key_type;
    name : string;
    kind : string;
    color : Utils.rgb;
    enabled_by_default : bool;
  }

let keys =
  [
    {
      ty = Signal;
      name = "Heap";
      kind = "HEAP_OCCUPANCY";
      color = (0.486, 1.988, 0.000);
      enabled_by_default = true;
    };
    {
      ty = Activity;
      name = "GC";
      kind = "GC";
      color = (1.000, 0.271, 0.000);
      enabled_by_default = true;
    };
    {
      ty = Activity;
      name = "Runtime";
      kind = "RUNTIME";
      color = (0.373, 0.620, 0.627);
      enabled_by_default = false;
    };
    {
      ty = Event;
      name = "Thread Copy";
      kind = "THREAD_COPY";
      color = (0.196, 0.804, 0.196);
      enabled_by_default = false;
    };
    {
      ty = Event;
      name = "GC Abort";
      kind = "GC_ABORT";
      color = (0.502, 0.000, 0.502);
      enabled_by_default = false;
    };
    {
      ty = Event;
      name = "Halt Request";
      kind = "HALT_REQ";
      color = (0.502, 0.000, 0.000);
      enabled_by_default = false;
    };
    {
      ty = Event;
      name = "Initialization";
      kind = "INIT";
      color = (0.498, 1.000, 0.831);
      enabled_by_default = false;
    };
  ]

let find_key name =
  List.find (fun k -> k.name = name) keys

let rgba_color k =
  let r, g, b = k.color in
  r, g, b, 1.

let find_color_rgba name =
  rgba_color (find_key name)

let heap_color =
  find_color_rgba "Heap"

let gc_color =
  find_color_rgba "GC"

let actions_for_key_type ty =
  match ty with
  | Signal ->
     Timeline.add_signal ~name:"Heap", Timeline.remove_signal
  | Activity ->
     Timeline.add_activity, Timeline.remove_activity
  | Event ->
     Timeline.add_event, Timeline.remove_event

let add_key tl name =
  let k = find_key name in
  let add, _ = actions_for_key_type k.ty in
  add ~kind:k.kind ~color:(rgba_color k) tl

let remove_key tl name =
  let k = find_key name in
  let _, remove = actions_for_key_type k.ty in
  remove ~kind:k.kind tl

let key_toggled
      ~timeline
      ~(model : GTree.tree_store)
      ~cname
      ~cenabled
      path =
  let row = model#get_iter path in
  let enabled = not @@ model#get ~row ~column:cenabled in
  let name = model#get ~row ~column:cname in
  model#set ~row ~column:cenabled enabled;
  if enabled then add_key timeline name else remove_key timeline name;
  ()

(* Initialize Gtk. DO NOT REMOVE! *)
let _ = GMain.init ()

let build_window ~filename () =
  let window = GWindow.window ~width ~height ~title:("Veezuh " ^ filename) () in
  ignore @@ window#connect#destroy ~callback:Main.quit;
  window

let build_activity_and_event_selector ~packing () =
  let open Gobject.Data in
  let columns = new GTree.column_list in
  let cname = columns#add string in
  let cenabled = columns#add boolean in
  let cvisible = columns#add boolean in
  let ccolor = columns#add (unsafe_boxed (Gobject.Type.from_name "GdkColor")) in

  let model = GTree.tree_store columns in

  (* Build the Gtk model. *)

  let signals = model#append () in
  let activities = model#append () in
  let events = model#append () in

  let add_row_for_key k =
    let parent =
      match k.ty with
      | Signal ->
         signals
      | Activity ->
         activities
      | Event ->
         events
    in
    let row = model#append ~parent () in
    model#set ~row ~column:cname k.name;
    model#set ~row ~column:cenabled k.enabled_by_default;
    model#set ~row ~column:cvisible true;
    model#set ~row ~column:ccolor (Utils.gdk_color_of_rgb_float k.color);
  in

  model#set ~row:signals ~column:cname "Signals";
  model#set ~row:signals ~column:cvisible false;

  model#set ~row:activities ~column:cname "Activities";
  model#set ~row:activities ~column:cvisible false;

  model#set ~row:events ~column:cname "Events";
  model#set ~row:events ~column:cvisible false;

  List.iter add_row_for_key keys;

  (* Build the columns. *)

  let view = GTree.view ~model ~packing () in

  let renderer = GTree.cell_renderer_text [] in
  let name_vc = GTree.view_column ~renderer:(renderer, ["text", cname]) () in
  name_vc#add_attribute renderer "background-gdk" ccolor;
  ignore @@ view#append_column name_vc;

  let renderer = GTree.cell_renderer_toggle [`ACTIVATABLE true] in
  let setup_toggled_callback callback =
    renderer#connect#toggled ~callback:(callback ~model ~cname ~cenabled)
  in
  let enabled_vc =
    GTree.view_column
      ~renderer:(renderer, ["active", cenabled]) ()
  in
  enabled_vc#add_attribute renderer "visible" cvisible;
  ignore @@ view#append_column enabled_vc;

  (* Set up the view. *)
  view#set_rules_hint true;
  view#set_headers_visible false;
  view#selection#set_mode `NONE;
  ignore @@ view#misc#connect#realize ~callback:view#expand_all;

  setup_toggled_callback

let build_keys ~packing () =
  let frame = GBin.frame ~label:"Keys" ~packing ~border_width:5 () in
  build_activity_and_event_selector ~packing:frame#add ()

let build_timeline ~packing trace =
  let number_of_processors = Trace.number_of_processors trace in

  let get_activities ~kind ~for_proc ~between ~min_duration =
    Trace.activities_between
      ~kind
      ~between
      ~min_duration (* place-holder *)
      ~proc:for_proc
      trace
  in

  let get_events ~kind ~for_proc ~between =
    Trace.events_between
      ~between
      ~kind
      ~proc:for_proc
      trace
  in

  let get_signal_max =
    let m = Trace.max_occupancy trace in
    fun ~kind -> m
  in

  let get_signal_samples ~kind ~between ~granularity =
    Trace.occupancy_between ~between ~granularity trace
  in

  let tl =
    Timeline.make
      ~global_epoch:(Trace.epoch trace)
      ~number_of_processors
      ~get_activities
      ~get_events
      ~get_signal_max
      ~get_signal_samples
      ~packing
      ()
  in
  add_key tl "GC";
  add_key tl "Heap";
  tl

let build_menu_entries ~menubar ~timeline () =
  let menu_factory = new GMenu.factory menubar in
  let accel_group = menu_factory#accel_group in

  (* File menu *)
  let file_menu = menu_factory#add_submenu "File" in
  let file_factory = new GMenu.factory file_menu ~accel_group in
  ignore @@ file_factory#add_item "Quit" ~key:GdkKeysyms._Q ~callback:Main.quit;

  (* View menu *)
  let view_menu = menu_factory#add_submenu "View" in
  let view_factory = new GMenu.factory view_menu ~accel_group in
  ignore @@
    view_factory#add_item
      "Zoom to default"
      ~key:GdkKeysyms._R
      ~callback:(fun _ -> Timeline.zoom_to_global timeline);
  ignore @@
    view_factory#add_item
      "Zoom to selection"
      ~key:GdkKeysyms._E
      ~callback:(fun _ -> Timeline.zoom_to_selection timeline);
  accel_group

let build_toplevel_window filename =
  let trace = Trace.from_sqlite_file filename in

  (* Window *)
  let window = build_window ~filename () in

  (* Top-level vertical box *)
  let vbox = GPack.vbox ~packing:window#add () in

  (* Menu bar *)
  let menubar = GMenu.menu_bar ~packing:vbox#pack () in

  (* Horizontal box *)
  let hbox = GPack.hbox ~packing:(vbox#pack ~expand:true) () in

  (* Event and activites pane *)
  let setup_callback = build_keys ~packing:hbox#pack () in

  (* Main timeline *)
  let timeline = build_timeline ~packing:(hbox#pack ~expand:true) trace in

  (* Connect the Key callback to the timeline. *)
  ignore @@ setup_callback (key_toggled ~timeline);

  (* Menu entries *)
  let accel_group = build_menu_entries ~menubar ~timeline () in
  window#add_accel_group accel_group;

  (* Display the window. *)
  window#show ();
  ()

let () =
  (* Create one window per file and enter Gtk+ main loop. *)
  for i = 1 to Array.length Sys.argv - 1 do
    build_toplevel_window Sys.argv.(i)
  done;
  GMain.main ()
