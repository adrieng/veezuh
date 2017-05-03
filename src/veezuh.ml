(* Graph widget test program. *)

open GMain
open Timeline

let width = 800

let height = 600

let events =
  [
    "Thread copying", "THREAD_COPY", (0.2, 0.3, 0.1, 0.8);
  ]

let create_event_check_buttons tl (f : GMenu.menu GMenu.factory) =
  let create_event_check_button (label, event_name, color) =
    let cb = f#add_check_item ~key:GdkKeysyms._T label in
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
      ~key:GdkKeysyms._E
      ~callback:(fun _ -> tl#zoom_to_selection ());

  create_event_check_buttons tl factory;

  window#add_accel_group accel_group;

  (* Display the windows and enter Gtk+ main loop *)
  window#show ();
  ()

let () =
  for i = 1 to Array.length Sys.argv - 1 do
    open_trace_window Sys.argv.(i)
  done;
  GMain.main ()
