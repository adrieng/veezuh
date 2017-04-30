(* Graph widget test program. *)

open GMain
open GdkKeysyms
open Timeline

let font_name = "-*-dejavu sans-medium-*-*-*-*-*"

let locale = GtkMain.Main.init ()

let font =
  try
    Gdk.Font.load font_name
  with
    Gpointer.Null -> failwith ("graph.ml: font " ^ font_name ^ ": not found")

let main ds =
  let window = GWindow.window ~width:640 ~height:480
                              ~title:"LablGtk graph widget demo" () in
  let vbox = GPack.vbox ~packing:window#add () in
  ignore @@ window#connect#destroy ~callback:Main.quit;

  (* Menu bar *)
  let menubar = GMenu.menu_bar ~packing:vbox#pack () in
  let factory = new GMenu.factory menubar in
  let accel_group = factory#accel_group in
  let file_menu = factory#add_submenu "File" in

  (* File menu *)
  let factory = new GMenu.factory file_menu ~accel_group in
  ignore @@ factory#add_item "Quit" ~key:_Q ~callback:Main.quit;

  (* Data. *)
  let array = Array.init 100 (fun _ -> Random.int 10) in

  (* Create a graph in the main area. *)
  let graph = new graph font ~packing:vbox#add array in
  graph#set_title "Random data";

  (* Display the windows and enter Gtk+ main loop *)
  window#add_accel_group accel_group;
  window#show ();
  Main.main ()

let () =
  let ds =
    if Array.length Sys.argv < 2
    then "./datasets/fib.opt.12631.sqlite"
    else Sys.argv.(1)
  in
  let ds = Dataset.from_sqlite_file ds in
  let start, finish = Dataset.time_range ds in
  Format.printf "Start: %f, Finish: %f, %d processors@."
    start
    finish
    (Dataset.number_of_processors ds)
    ;
  main ds
