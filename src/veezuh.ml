(* Graph widget test program. *)

open GMain
open Timeline

let width = 800

let height = 600

let events =
  [
    "Thread copying", "THREAD_COPY", (0.755, 0.8, 0.1, 0.8);
  ]

let event_boxes = ref []

let create_event_check_buttons (f : GMenu.menu GMenu.factory) =
  let create_event_check_button (label, _, _) =
    event_boxes :=
      f#add_check_item
        ~key:GdkKeysyms._T
        "Thread creation"
      :: !event_boxes
  in
  List.iter create_event_check_button events

let connect_callbacks tl =
  let connect_callback cb (_, event_name, color) =
    ignore @@
      cb#connect#toggled ~callback:(fun () -> tl#toggle_kind event_name color)
  in
  List.iter2 connect_callback !event_boxes events

let create_timeline (b : GPack.box) trace =
  let vbox = GPack.vbox ~packing:(b#pack ~expand:true) () in
  ignore @@ GMisc.label ~markup:"<b><u>Timeline</u></b>" ~packing:vbox#pack ();
  new timeline ~packing:(vbox#pack ~expand:true ~fill:true) trace

let main filename trace =
  (* Initialize Gtk. DO NOT REMOVE! *)
  ignore @@ GMain.init ();

  (* Window *)
  let window = GWindow.window ~width ~height ~title:("Veezuh " ^ filename) () in
  let vbox = GPack.vbox ~packing:window#add () in
  ignore @@ window#connect#destroy ~callback:Main.quit;

  (* Menu bar *)
  let menubar = GMenu.menu_bar ~packing:vbox#pack () in
  let factory = new GMenu.factory menubar in
  let accel_group = factory#accel_group in
  let file_menu = factory#add_submenu "File" in
  let view_menu = factory#add_submenu "View" in

  (* File menu *)
  let factory = new GMenu.factory file_menu ~accel_group in
  ignore @@ factory#add_item "Quit" ~key:GdkKeysyms._Q ~callback:Main.quit;

  let tl = create_timeline vbox trace in

  (* View menu *)
  let factory = new GMenu.factory view_menu ~accel_group in
  ignore @@
    factory#add_item
      "Zoom to default"
      ~key:GdkKeysyms._R
      ~callback:(fun _ -> tl#zoom_to_default ());
  ignore @@
    factory#add_item
      "Zoom to selection"
      ~key:GdkKeysyms._V
      ~callback:(fun _ -> tl#zoom_to_selection ());

  create_event_check_buttons factory;

  window#add_accel_group accel_group;

  (* Connect the event buttons callback to the timeline. *)
  connect_callbacks tl;

  (* Display the windows and enter Gtk+ main loop *)
  window#show ();
  Main.main ()

let () =
  let fn =
    if Array.length Sys.argv < 2
    then (Printf.eprintf "Usage: %s file.sqlite" Sys.argv.(0); exit 1)
    else Sys.argv.(1)
  in

  let trace = Trace.from_sqlite_file fn in
  main fn trace
