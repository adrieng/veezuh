(* Graph widget test program. *)

open GMain

let width = 800

let height = 600

let events =
  [
    "Thread copying", "THREAD_COPY", (0.2, 0.3, 0.1, 0.8), GdkKeysyms._T;
    "GC aborts", "GC_ABORT", (0.6, 0.05, 0.1, 0.8), GdkKeysyms._A;
  ]

(* Initialize Gtk. DO NOT REMOVE! *)
let _ = GMain.init ()

let open_trace_window filename =
  let trace = Trace.from_sqlite_file filename in
  let number_of_processors = Trace.number_of_processors trace in

  (* Window *)
  let window = GWindow.window ~width ~height ~title:("Veezuh " ^ filename) () in
  let vbox = GPack.vbox ~packing:window#add () in
  ignore @@ window#connect#destroy ~callback:Main.quit;

  (* Menu bar *)
  let menubar = GMenu.menu_bar ~packing:vbox#pack () in
  let menu_factory = new GMenu.factory menubar in
  let accel_group = menu_factory#accel_group in

  (* File menu *)
  let file_menu = menu_factory#add_submenu "File" in
  let file_factory = new GMenu.factory file_menu ~accel_group in
  ignore @@ file_factory#add_item "Quit" ~key:GdkKeysyms._Q ~callback:Main.quit;

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

  let hbox = GPack.hbox ~packing:(vbox#pack ~expand:true) () in

  let tl =
    Timeline.make
      ~global_epoch:(Trace.epoch trace)
      ~number_of_processors
      ~get_activities
      ~get_events
      ~packing:(hbox#pack ~expand:true)
      ()
  in
  Timeline.add_activity ~kind:"GC" ~color:(1.0, 0.2, 0.0, 1.) tl;
  Timeline.add_event ~kind:"THREAD_COPY" ~color:(0.6, 0.1, 0.2, 1.) tl;

  (* View menu *)
  let view_menu = menu_factory#add_submenu "View" in
  let view_factory = new GMenu.factory view_menu ~accel_group in
  ignore @@
    view_factory#add_item
      "Zoom to default"
      ~key:GdkKeysyms._R
      ~callback:(fun _ ->
        (* tl#zoom_to_default (); *)
        Timeline.zoom_to_global tl);
  ignore @@
    view_factory#add_item
      "Zoom to selection"
      ~key:GdkKeysyms._E
      ~callback:(fun _ ->
        (* tl#zoom_to_selection (); *)
        Timeline.zoom_to_selection tl
      );

  window#add_accel_group accel_group;

  (* Display the windows and enter Gtk+ main loop *)
  window#show ();
  ()

let () =
  for i = 1 to Array.length Sys.argv - 1 do
    open_trace_window Sys.argv.(i)
  done;
  GMain.main ()
