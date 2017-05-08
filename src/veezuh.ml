(* Keys (things to display) *)

type window =
  {
    trace : Trace.t;
    rows : Timeline.row list;
  }

let row_name_for_processor ~proc =
  Printf.sprintf "P%d" proc

let find_row_for_processor w ~proc =
  let name = row_name_for_processor ~proc in
  List.find (fun r -> Timeline.get_row_name r = name) w.rows

let build_heap_keys trace =
  let heap =
    Timeline.make_row
      ~name:"Heap"
      ~rank:0
      ~height:150
      ~background:Utils.transparent
      ~visible:true
  in

  let occupancy =
    let kind =
      Timeline.Signal
        {
          Timeline.max = (fun () -> Trace.max_occupancy trace);
          Timeline.samples = Trace.occupancy_between trace;
          Timeline.alpha_mult = 0.1;
        }
    in
    Timeline.make_key
      ~name:"Occupancy"
      ~kind
      ~color:(0.486, 1.988, 0.000, 1.)
      ~visible:true
  in

  Timeline.row_add_key heap occupancy;

  heap

let build_keys_for_processor trace ~proc =
  let ratio =
    {
      Timeline.max = (fun () -> Trace.max_ratio trace ~proc);
      Timeline.samples = Trace.ratio_between trace ~proc;
      Timeline.alpha_mult = 0.1;
    }
  in

  let get_events kind =
    {
      Timeline.events = Trace.events_between trace ~proc ~kind;
    }
  in
  let get_activites kind =
    {
      Timeline.activities = Trace.activities_between trace ~proc ~kind;
    }
  in

  let proc_row =
    Timeline.make_row
      ~name:(row_name_for_processor proc)
      ~rank:(proc + 1)
      ~height:50
      ~background:Utils.grey_background
      ~visible:true
  in

  let keys =
    let open Timeline in
    [
      make_key
        ~name:"Ratio"
        ~kind:(Signal ratio)
        ~color:(0.855, 0.647, 0.125, 1.)
        ~visible:true;
      make_key
        ~name:"GC"
        ~kind:(Activity (get_activites "GC"))
        ~color:(1.000, 0.271, 0.000, 0.75)
        ~visible:true;
      make_key
        ~name:"Runtime"
        ~kind:(Activity (get_activites "RUNTIME"))
        ~color:(0.373, 0.620, 0.627, 0.75)
        ~visible:false;
      make_key
        ~name:"Thread Copy"
        ~kind:(Event (get_events "THREAD_COPY"))
        ~color:(0.196, 0.804, 0.196, 1.)
        ~visible:false;
      make_key
        ~name:"GC Abort"
        ~kind:(Event (get_events "GC_ABORT"))
        ~color:(0.502, 0.000, 0.502, 1.)
        ~visible:false;
      make_key
        ~name:"Halt Request"
        ~kind:(Event (get_events "HALT_REQ"))
        ~color:(0.502, 0.000, 0.000, 1.)
        ~visible:false;
      make_key
        ~name:"Initialization"
        ~kind:(Event (get_events "INIT"))
        ~color:(0.498, 1.000, 0.831, 1.)
        ~visible:false;
    ]
  in

  List.(keys |> rev |> iter (Timeline.row_add_key proc_row));
  proc_row

(* Main graphical interface *)

(* Initialize Gtk. DO NOT REMOVE! *)
let _ = GMain.init ()

open GMain

(* Global parameters *)

let width = 800

let height = 600

(* GtkTreeView related things *)

type treeview_info =
  {
    store : GTree.tree_store;
    view : GTree.view;
    cname : string GTree.column;
    cenabled : bool GTree.column;
    cvisible : bool GTree.column;
    ccolor : Gdk.color GTree.column;
    set_on_toggle : (Gtk.tree_path -> unit) -> unit;
  }

let key_toggled
      ~info
      ~timeline
      path =
  let row = info.store#get_iter path in
  let enabled = not @@ info.store#get ~row ~column:info.cenabled in
  info.store#set ~row ~column:info.cenabled enabled;
  let name = info.store#get ~row ~column:info.cname in

  begin match info.store#iter_parent row with
  | None ->
     (* User clicked on a row *)
     let row = Timeline.find_row timeline name in
     Timeline.set_row_visible row ~visible:enabled;
     Timeline.refresh timeline
  | Some prow ->
     (* User clicked on a key *)
     let pname = info.store#get ~row:prow ~column:info.cname in
     let row = Timeline.find_row timeline pname in
     let key = Timeline.row_find_key row name in
     Timeline.set_key_visible key ~visible:enabled;
     Timeline.refresh timeline
  end;
  Timeline.refresh timeline

let build_window ~filename () =
  let window = GWindow.window ~width ~height ~title:("Veezuh " ^ filename) () in
  ignore @@ window#connect#destroy ~callback:Main.quit;
  window

let build_model_from_timeline_rows tl m =
  let build_model_row_from_tl_row r =
    let open Timeline in
    let row = m.store#append () in
    m.store#set ~row ~column:m.cname (get_row_name r);
    m.store#set ~row ~column:m.cvisible true;

    let build_model_row_from_key k =
      let open Timeline in
      let color = Utils.gdk_color_of_rgba (get_key_color k) in
      let krow = m.store#append ~parent:row () in
      m.store#set ~row:krow ~column:m.cname (get_key_name k);
      m.store#set ~row:krow ~column:m.cenabled (get_key_visible k);
      m.store#set ~row:krow ~column:m.cvisible true;
      m.store#set ~row:krow ~column:m.ccolor color
    in
    Timeline.row_iter_keys build_model_row_from_key r
  in

  Timeline.iter_rows build_model_row_from_tl_row tl;
  ()

let build_info_from_timeline_rows rows ~packing () =
  let open Gobject.Data in
  let columns = new GTree.column_list in
  let cname = columns#add string in
  let cenabled = columns#add boolean in
  let cvisible = columns#add boolean in
  let ccolor = columns#add (unsafe_boxed (Gobject.Type.from_name "GdkColor")) in

  let store = GTree.tree_store columns in
  let view = GTree.view ~model:store ~packing () in

  (* Build the Gtk model from the timeline rows. *)

  let add_gtk_row_of_tl_row pr =
    let open Timeline in
    let row = store#append () in
    store#set ~row ~column:cname (get_row_name pr);
    store#set ~row ~column:cvisible true;
    store#set ~row ~column:cenabled (get_row_visible pr);

    let add_row_for_key k =
      let open Timeline in
      let color = Utils.gdk_color_of_rgba (get_key_color k) in
      let krow = store#append ~parent:row () in
      store#set ~row:krow ~column:cname (get_key_name k);
      store#set ~row:krow ~column:cenabled (get_key_visible k);
      store#set ~row:krow ~column:cvisible true;
      store#set ~row:krow ~column:ccolor color
    in
    Timeline.row_iter_keys add_row_for_key pr
  in

  List.iter add_gtk_row_of_tl_row rows;

  (* Build the view. *)

  let renderer = GTree.cell_renderer_text [] in
  let name_vc = GTree.view_column ~renderer:(renderer, ["text", cname]) () in
  name_vc#add_attribute renderer "background-gdk" ccolor;
  ignore @@ view#append_column name_vc;

  let renderer = GTree.cell_renderer_toggle [`ACTIVATABLE true] in
  let set_on_toggle callback =
    ignore @@ renderer#connect#toggled ~callback
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

  {
    store;
    view;
    cname;
    cenabled;
    cvisible;
    ccolor;
    set_on_toggle;
  }

let build_key_pane trace ~packing () =
  let frame = GBin.frame ~label:"Keys" ~packing ~border_width:5 () in
  let rows = ref [] in
  for proc = Trace.number_of_processors trace - 1 downto 0 do
    rows := build_keys_for_processor trace ~proc :: !rows
  done;
  rows := build_heap_keys trace :: !rows;
  let info = build_info_from_timeline_rows !rows ~packing:frame#add () in
  !rows, info

let build_timeline rows ~packing trace =
  let timeline =
    Timeline.make
      ~global_epoch:(Trace.epoch trace)
      ~packing
      ()
  in
  List.iter (Timeline.add_row timeline) rows;
  timeline

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

  (* Key pane *)
  let rows, info = build_key_pane trace ~packing:hbox#pack () in

  (* Main timeline *)
  let timeline = build_timeline rows ~packing:(hbox#pack ~expand:true) trace in

  (* Connect the Key callback to the timeline. *)
  ignore @@ info.set_on_toggle (key_toggled ~info ~timeline);

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
