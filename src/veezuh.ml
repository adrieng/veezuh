(* Graph widget test program. *)

open GMain

let width = 800

let height = 600

let activity_entries =
  [
    "GC",      ("GC",      (1.000, 0.271, 0.000),  true);
    "Runtime", ("RUNTIME", (0.373, 0.620, 0.627), false);
  ]

let event_entries =
  [
    "Thread Copy", ("THREAD_COPY", (0.196, 0.804, 0.196), false);
       "GC Abort", ("GC_ABORT",    (0.502, 0.000, 0.502), false);
   "Halt Request", ("HALT_REQ",    (0.502, 0.000, 0.000), false);
  ]

let gc_color =
  let _, (_, (r, g, b), _) = List.hd activity_entries in
  (r, g, b, 1.)

let activity_or_event_toggled
      ~timeline
      ~(model : GTree.tree_store)
      ~cname
      ~cenabled
      path =
  let find_activity_or_event name =
    try true, List.assoc name activity_entries
    with Not_found -> false, List.assoc name event_entries
  in

  let add name =
    let is_activity, (kind, (r, g, b), _) = find_activity_or_event name in
    Timeline.(if is_activity then add_activity else add_event)
      ~kind
      ~color:(r, g, b, 1.)
      timeline
  in

  let remove name =
    let is_activity, (kind, _, _) = find_activity_or_event name in
    Timeline.(if is_activity then remove_activity else remove_event)
      ~kind
      timeline
  in

  let row = model#get_iter path in
  let enabled = not @@ model#get ~row ~column:cenabled in
  let name = model#get ~row ~column:cname in
  model#set ~row ~column:cenabled enabled;
  if enabled then add name else remove name;
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

  let add_rows_for_entries ~parent entries =
    let add_row (name, (_, color, enabled)) =
      let row = model#append ~parent () in
      model#set ~row ~column:cname name;
      model#set ~row ~column:cenabled enabled;
      model#set ~row ~column:cvisible true;
      model#set ~row ~column:ccolor (Utils.gdk_color_of_rgb_float color);
    in
    List.iter add_row entries
  in

  let activities = model#append () in
  model#set ~row:activities ~column:cname "Activities";
  model#set ~row:activities ~column:cvisible false;
  add_rows_for_entries ~parent:activities activity_entries;

  let events = model#append () in
  model#set ~row:events ~column:cname "Events";
  model#set ~row:events ~column:cvisible false;
  add_rows_for_entries ~parent:events event_entries;

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

  let tl =
    Timeline.make
      ~global_epoch:(Trace.epoch trace)
      ~number_of_processors
      ~get_activities
      ~get_events
      ~packing
      ()
  in
  Timeline.add_activity ~kind:"GC" ~color:gc_color tl;
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
  ignore @@ setup_callback (activity_or_event_toggled ~timeline);

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
