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

  let lch =
    {
      Timeline.max = (fun () -> Trace.max_locally_collectible_heap trace ~proc);
      Timeline.samples = Trace.locally_collectible_heap_between trace ~proc;
      Timeline.alpha_mult = 0.1;
    }
  in

  let lc =
    {
      Timeline.max = (fun () -> Trace.max_locally_collectible trace ~proc);
      Timeline.samples = Trace.locally_collectible_between trace ~proc;
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
        ~visible:false;
      make_key
        ~name:"Locally collectible heap"
        ~kind:(Signal lch)
        ~color:(0.824, 0.706, 0.549, 1.)
        ~visible:false;
      make_key
        ~name:"Locally collectible"
        ~kind:(Signal lc)
        ~color:(0.737, 0.561, 0.561, 1.)
        ~visible:false;
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
     match pname with
     | "Per-processor" ->
        (* Toggle for all events *)
        let toggle row =
          match Timeline.row_find_key row name with
          | key ->
             Timeline.set_key_visible key enabled
          | exception Not_found ->
             ()
        in
        Timeline.iter_rows toggle timeline
     | _ ->
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

let add_model_row_from_key info parent k =
  let open Timeline in
  let color = Utils.gdk_color_of_rgba (get_key_color k) in
  let krow = info.store#append ~parent () in
  info.store#set ~row:krow ~column:info.cname (get_key_name k);
  info.store#set ~row:krow ~column:info.cenabled (get_key_visible k);
  info.store#set ~row:krow ~column:info.cvisible true;
  info.store#set ~row:krow ~column:info.ccolor color

let add_model_row_from_tl_row info r =
  let open Timeline in
  let row = info.store#append () in
  info.store#set ~row ~column:info.cname (get_row_name r);
  info.store#set ~row ~column:info.cvisible true;
  info.store#set ~row ~column:info.cenabled (get_row_visible r);
  row

let add_model_row_from_tl_row_with_keys info r =
  let row = add_model_row_from_tl_row info r in
  Timeline.row_iter_keys (add_model_row_from_key info row) r

let add_model_row_from_key_not_seen info parent seen k =
  let name = Timeline.get_key_name k in
  if Utils.SSet.mem name seen
  then seen
  else
    begin
      add_model_row_from_key info parent k;
      Utils.SSet.add name seen
    end

let add_global_model_row_from_tl_rows info rows =
  let row = info.store#append () in
  info.store#set ~row ~column:info.cname "Per-processor";
  info.store#set ~row ~column:info.cvisible false;
  let add seen r =
    Timeline.row_fold_keys (add_model_row_from_key_not_seen info row) seen r
  in
  ignore @@ List.fold_left add Utils.SSet.empty rows

let build_info_from_timeline_rows ~packing () =
  let open Gobject.Data in
  let columns = new GTree.column_list in
  let cname = columns#add string in
  let cenabled = columns#add boolean in
  let cvisible = columns#add boolean in
  let ccolor = columns#add (unsafe_boxed (Gobject.Type.from_name "GdkColor")) in
  let store = GTree.tree_store columns in
  let view = GTree.view ~model:store ~packing () in

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
  (* Build graphical components *)
  let frame = GBin.frame ~label:"Keys" ~packing ~border_width:5 () in
  let info = build_info_from_timeline_rows ~packing:frame#add () in

  (* Populate the model for the TreeView *)
  let heap_tl_row = build_heap_keys trace in
  let tl_rows = ref [] in
  for proc = Trace.number_of_processors trace - 1 downto 0 do
    tl_rows := build_keys_for_processor trace ~proc :: !tl_rows
  done;

  ignore @@ add_model_row_from_tl_row_with_keys info heap_tl_row;
  add_global_model_row_from_tl_rows info !tl_rows;
  List.iter (fun k -> ignore @@ add_model_row_from_tl_row info k) !tl_rows;

  heap_tl_row :: !tl_rows, info

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

let benchmark filen =
  let trace = Trace.from_sqlite_file filen in
  let epoch = Trace.epoch trace in

  let time_call f =
    let start = Unix.gettimeofday () in
    f ();
    let stop = Unix.gettimeofday () in
    Printf.printf "Elapsed: %f second(s)\n" (stop -. start)
  in

  time_call
    (fun () ->
      let ratios =
        Trace.ratio_between
          trace
          ~proc:0
          ~between:epoch
          ~granularity:0.000001
      in
      Printf.printf "File %s: %d ratios.\n" filen (List.length ratios));

  time_call
    (fun () ->
      let act =
        Trace.activities_between
          trace
          ~kind:"GC"
          ~proc:0
          ~between:epoch
          ~min_duration:0.000001
      in
      Printf.printf "File %s: %d GC periods.\n" filen (List.length act));

  time_call
    (fun () ->
      let act =
        Trace.activities_between
          trace
          ~kind:"Runtime"
          ~proc:0
          ~between:epoch
          ~min_duration:0.000001
      in
      Printf.printf "File %s: %d runtime periods.\n" filen (List.length act));
  ()

let () =
  let files = ref [] in

  let args =
    [
      "-benchmark", Arg.String benchmark, "Benchmark trace file";
    ]
  in

  let usage =
    "Trace visualization program"
  in

  Arg.parse args (fun s -> files := s :: !files) usage;

  (* Create one window per file and enter Gtk+ main loop. *)
  if !files <> [] then
    begin
      List.iter build_toplevel_window !files;
      GMain.main ()
    end;
  ()
