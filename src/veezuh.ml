(* Graph widget test program. *)

open GMain
open Timeline

let width = 800

let height = 600

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

  (* Timeline and label *)
  ignore @@ GMisc.label ~markup:"<b><u>Timeline</u></b>" ~packing:vbox#pack ();
  let tl = new timeline ~packing:(vbox#pack ~expand:true) trace in

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
  window#add_accel_group accel_group;

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
