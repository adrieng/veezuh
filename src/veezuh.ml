(* Graph widget test program. *)

open GMain
open Timeline

let width = 800

let height = 600

let main trace =
  (* Initialize Gtk. DO NOT REMOVE! *)
  ignore @@ GMain.init ();

  (* Window *)
  let window = GWindow.window ~width ~height ~title:"Veezuh" () in
  let vbox = GPack.vbox ~packing:window#add () in
  ignore @@ window#connect#destroy ~callback:Main.quit;

  (* Menu bar *)
  let menubar = GMenu.menu_bar ~packing:vbox#pack () in
  let factory = new GMenu.factory menubar in
  let accel_group = factory#accel_group in
  let file_menu = factory#add_submenu "File" in

  (* File menu *)
  let factory = new GMenu.factory file_menu ~accel_group in
  ignore @@ factory#add_item "Quit" ~key:GdkKeysyms._Q ~callback:Main.quit;
  window#add_accel_group accel_group;

  (* Lable *)
  ignore @@ GMisc.label ~markup:"<b><u>Timeline</u></b>" ~packing:vbox#pack ();

  (* Timeline *)
  let _ = new timeline ~packing:(vbox#pack ~expand:true) trace in

  (* Display the windows and enter Gtk+ main loop *)
  window#show ();
  Main.main ()

let () =
  let fn =
    if Array.length Sys.argv < 2
    then "./datasets/fib.opt.12631.sqlite"
    else Sys.argv.(1)
  in
  main @@ Trace.from_sqlite_file fn
