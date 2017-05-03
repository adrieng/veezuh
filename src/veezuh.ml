(* Graph widget test program. *)

open GMain
open Timeline

let width = 800

let height = 600

let events =
  [
    "Thread copying", "THREAD_COPY", (0.2, 0.3, 0.1, 0.8), GdkKeysyms._T;
    "GC aborts", "GC_ABORT", (0.6, 0.05, 0.1, 0.8), GdkKeysyms._A;
  ]

let create_event_check_buttons tl (f : GMenu.menu GMenu.factory) =
  let create_event_check_button (label, event_name, color, key) =
    let cb = f#add_check_item ~key label in
    let callback () = tl#toggle_kind event_name color in
    ignore @@ cb#connect#toggled ~callback
  in
  List.iter create_event_check_button events

let create_timeline (b : GPack.box) trace =
  let vbox = GPack.vbox ~packing:(b#pack ~expand:true) () in
  ignore @@ GMisc.label ~markup:"<b><u>Timeline</u></b>" ~packing:vbox#pack ();
  new timeline ~packing:(vbox#pack ~expand:true ~fill:true) trace

(* Initialize Gtk. DO NOT REMOVE! *)
let _ = GMain.init ()

let open_trace_window filename =
  let trace = Trace.from_sqlite_file filename in

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

  let tl = create_timeline vbox trace in

  (* View menu *)
  let view_menu = menu_factory#add_submenu "View" in
  let view_factory = new GMenu.factory view_menu ~accel_group in
  ignore @@
    view_factory#add_item
      "Zoom to default"
      ~key:GdkKeysyms._R
      ~callback:(fun _ -> tl#zoom_to_default ());
  ignore @@
    view_factory#add_item
      "Zoom to selection"
      ~key:GdkKeysyms._E
      ~callback:(fun _ -> tl#zoom_to_selection ());
  let event_submenu = view_factory#add_submenu "Events" in
  let event_factory = new GMenu.factory event_submenu ~accel_group in
  create_event_check_buttons tl event_factory;

  window#add_accel_group accel_group;

  (* Display the windows and enter Gtk+ main loop *)
  window#show ();
  ()

let () =
  for i = 1 to Array.length Sys.argv - 1 do
    open_trace_window Sys.argv.(i)
  done;
  GMain.main ()
