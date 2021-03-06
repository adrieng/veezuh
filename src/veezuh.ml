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
      ~background:Utils.light_background
      ~visible:true
  in

  let chunkp_occupancy =
    let kind =
      Timeline.Signal
        {
          Timeline.max = float @@ Trace.max_chunkp_size trace;
          Timeline.samples = Trace.chunkp_occupancy_between trace ();
          Timeline.alpha_mult = 0.1;
        }
    in
    Timeline.make_key
      ~name:"Chunk Pool Occupancy"
      ~kind
      ~color:(0.486, 1.988, 0.000, 1.)
      ~visible:true
  in

  let heap_occupancy =
    let kind =
      Timeline.Signal
        {
          Timeline.max = float @@ Trace.max_heap_size trace;
          Timeline.samples = Trace.heap_occupancy_between trace ();
          Timeline.alpha_mult = 0.1;
        }
    in
    Timeline.make_key
      ~name:"Heap Occupancy"
      ~kind
      ~color:(0.604, 0.804, 0.196, 1.)
      ~visible:false
  in

  let heap_size =
    let kind =
      Timeline.Signal
        {
          Timeline.max = float @@ Trace.max_heap_size trace;
          Timeline.samples = Trace.heap_size_between trace ();
          Timeline.alpha_mult = 0.1;
        }
    in
    Timeline.make_key
      ~name:"Heap Size"
      ~kind
      ~color:(0.596, 0.984, 0.596, 1.)
      ~visible:false
  in

  let alloc_occupancy =
    let kind =
      Timeline.Signal
        {
          Timeline.max = float @@ Trace.max_array_mem trace;
          Timeline.samples = Trace.array_mem_between trace ();
          Timeline.alpha_mult = 0.1;
        }
    in
    Timeline.make_key
      ~name:"Array Allocations"
      ~kind
      ~color:(0.000, 1.000, 0.498, 0.8)
      ~visible:false
  in

  Timeline.row_add_key heap alloc_occupancy;
  Timeline.row_add_key heap heap_size;
  Timeline.row_add_key heap heap_occupancy;
  Timeline.row_add_key heap chunkp_occupancy;

  heap

let build_keys_for_processor trace ~proc =
  let lch =
    {
      Timeline.max = Trace.max_locally_collectible_heap trace ~proc ();
      Timeline.samples = Trace.locally_collectible_heap_between trace ~proc ();
      Timeline.alpha_mult = 0.1;
    }
  in

  let lc =
    {
      Timeline.max = Trace.max_locally_collectible trace ~proc ();
      Timeline.samples = Trace.locally_collectible_between trace ~proc ();
      Timeline.alpha_mult = 0.1;
    }
  in

  let copy =
    let copied_between =
      Trace.signal_between trace ~kind:"COPY" ~selector:"arg1" ~proc ()
    in
    {
      Timeline.max = Trace.max_copy trace ~proc ();
      Timeline.samples = copied_between;
      Timeline.alpha_mult = 0.1;
    }
  in

  let ratio_max = Trace.max_ratio trace ~proc in
  let ratio =
    {
      Timeline.max = ratio_max;
      Timeline.samples = Trace.ratio_between trace ~proc ();
      Timeline.alpha_mult = 0.1;
    }
  in

  let cratio =
    let cratio_max = Trace.max_control_ratio trace ~proc () in
    let samples ~between ~granularity =
      [(between.Range.l, cratio_max)]
    in
    {
      Timeline.max = ratio_max;
      Timeline.samples = samples;
      Timeline.alpha_mult = 0.1;
    }
  in

  let get_events kind =
    let events ~granularity =
        Trace.events_between
          trace
          ~granularity
          ~proc
          ~kind
          ()
    in
    {
      Timeline.events = events;
    }
  in
  let get_activites ~name ~enter ~leave =
    {
      Timeline.activities =
        Trace.activities_between
          trace
          ~name
          ~enter
          ~leave
          ~proc
          ();
    }
  in

  let proc_row =
    Timeline.make_row
      ~name:(row_name_for_processor proc)
      ~rank:(proc + 1)
      ~height:75
      ~background:Utils.grey_background
      ~visible:true
  in

  let keys =
    let open Timeline in
    [
      make_key
        ~name:"LCHS"
        ~kind:(Signal lch)
        ~color:(0.824, 0.706, 0.549, 1.)
        ~visible:false;
      make_key
        ~name:"LCS"
        ~kind:(Signal lc)
        ~color:(0.737, 0.561, 0.561, 1.)
        ~visible:false;
      make_key
        ~name:"LCHS/LCS"
        ~kind:(Signal ratio)
        ~color:(0.855, 0.647, 0.125, 1.)
        ~visible:false;
      make_key
        ~name:"LCHS/LCS control"
        ~kind:(Signal cratio)
        ~color:(0.755, 0.247, 0.125, 1.)
        ~visible:false;
      make_key
        ~name:"GC"
        ~kind:(Activity
                 (get_activites
                    ~name:"GC"
                    ~enter:"GC_ENTER"
                    ~leave:"GC_LEAVE"))
        ~color:(1.000, 0.647, 0.000, 0.9)
        ~visible:true;
      make_key
        ~name:"Bytes Copied"
        ~kind:(Signal copy)
        ~color:(0.596, 0.984, 0.596, 1.)
        ~visible:false;
      make_key
        ~name:"Runtime"
        ~kind:(Activity
                 (get_activites
                    ~name:"Runtime"
                    ~enter:"RUNTIME_ENTER"
                    ~leave:"RUNTIME_LEAVE"))
        ~color:(0.373, 0.620, 0.627, 0.75)
        ~visible:false;
      make_key
        ~name:"Initialization"
        ~kind:(Event (get_events "INIT"))
        ~color:(0.498, 1.000, 0.831, 1.)
        ~visible:true;
      make_key
        ~name:"Launch"
        ~kind:(Event (get_events "LAUNCH"))
        ~color:(0.780, 0.082, 0.522, 1.)
        ~visible:true;
      make_key
        ~name:"Thread Copy"
        ~kind:(Event (get_events "THREAD_COPY"))
        ~color:(0.196, 0.804, 0.196, 1.)
        ~visible:false;
      make_key
        ~name:"Halt Request"
        ~kind:(Event (get_events "HALT_REQ"))
        ~color:(0.502, 0.000, 0.000, 1.)
        ~visible:false;
      make_key
        ~name:"Halt Ack"
        ~kind:(Event (get_events "HALT_ACK"))
        ~color:(0.855, 0.647, 0.125, 1.)
        ~visible:false;
      make_key
        ~name:"Global Critical Section"
        ~kind:(Activity
                 (get_activites
                    ~name:"GSection"
                    ~enter:"GSECTION_BEGIN_ENTER"
                    ~leave:"GSECTION_END_LEAVE"))
        ~color:(0.729, 0.333, 0.827, 0.6)
        ~visible:false;
      make_key
        ~name:"Lock Contention"
        ~kind:(Event (get_events "LOCK_TAKE_ENTER"))
        ~color:(1.000, 0.000, 0.000, 0.6)
        ~visible:false;
      make_key
        ~name:"Lock Taking"
        ~kind:(Activity
                 (get_activites
                    ~name:"LockTaking"
                    ~enter:"LOCK_TAKE_ENTER"
                    ~leave:"LOCK_TAKE_LEAVE"))
        ~color:(1.000, 0.271, 0.000, 0.6)
        ~visible:false;
      make_key
        ~name:"RWLock R Take"
        ~kind:(Event (get_events "RWLOCK_R_TAKE"))
        ~color:(1.000, 0.388, 0.278, 1.0)
        ~visible:false;
      make_key
        ~name:"RWLock R Release"
        ~kind:(Event (get_events "RWLOCK_R_RELEASE"))
        ~color:(0.804, 0.361, 0.361, 1.0)
        ~visible:false;
      make_key
        ~name:"RWLock W Take"
        ~kind:(Event (get_events "RWLOCK_W_TAKE"))
        ~color:(1.000, 0.078, 0.576, 1.0)
        ~visible:false;
      make_key
        ~name:"RWLock W Release"
        ~kind:(Event (get_events "RWLOCK_W_RELEASE"))
        ~color:(0.780, 0.082, 0.522, 1.0)
        ~visible:false;
      make_key
        ~name:"Promotion"
        ~kind:(Event (get_events "PROMOTION_ENTER"))
        ~color:(0.867, 0.627, 0.867, 1.)
        ~visible:false;
      make_key
        ~name:"Promotion Period"
        ~kind:(Activity
                 (get_activites
                    ~name:"Promotion"
                    ~enter:"PROMOTION_ENTER"
                    ~leave:"PROMOTION_LEAVE"))
        ~color:(0.933, 0.510, 0.933, 0.7)
        ~visible:false;
      make_key
        ~name:"Array Allocation"
        ~kind:(Activity
                 (get_activites
                    ~name:"ArrayAllocate"
                    ~enter:"ARRAY_ALLOCATE_ENTER"
                    ~leave:"ARRAY_ALLOCATE_LEAVE"))
        (* ~color:(1.000, 0.855, 0.725, 0.8) *)
        ~color:(0.255, 0.412, 0.882, 0.8)
        ~visible:false;
      make_key
        ~name:"Merged Heap"
        ~kind:(Event (get_events "MERGED_HEAP"))
        ~color:(0.184, 0.310, 0.310, 1.0)
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
     | "Heap" ->
        let row = Timeline.find_row timeline pname in
        let key = Timeline.row_find_key row name in
        Timeline.set_key_visible key ~visible:enabled;
        Timeline.refresh timeline
     | _ ->
        (* Toggle for all events *)
        let toggle row =
          match Timeline.row_find_key row name with
          | key ->
             Timeline.set_key_visible key enabled
          | exception Not_found ->
             ()
        in
        Timeline.iter_rows toggle timeline
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

  let make_category_row name =
    let cat_row = info.store#append ~parent:row () in
    info.store#set ~row:cat_row ~column:info.cname name;
    info.store#set ~row:cat_row ~column:info.cvisible false;
    cat_row
  in

  let global_row = make_category_row "Global" in
  let concurrency_row = make_category_row "Concurrency" in
  let hh_row = make_category_row "Hierarchical Heap" in

  let find_parent_row_for_key key =
    match Timeline.get_key_name key with
    | "GC" | "Runtime"
    | "Thread Copy"
    | "Initialization" | "Launch" | "Halt Request" | "Halt Ack"
    | "Array Allocation"
    | "Bytes Copied"
      ->
       global_row

    | "Lock Taking" | "Lock Contention"
    | "Global Critical Section"
    | "RWLock R Take" | "RWLock R Release"
    | "RWLock W Take" | "RWLock W Release"
    | "Promotion" | "Promotion Period"
      ->
       concurrency_row
    | "LCHS" | "LCS" | "LCHS/LCS" | "LCHS/LCS control"
    | "GC Abort" | "Merged Heap"
      ->
       hh_row
    | _ ->
       row
  in
  let add seen r =
    let add seen k =
      let parent = find_parent_row_for_key k in
      add_model_row_from_key_not_seen info parent seen k
    in
    Timeline.row_fold_keys add seen r
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
  let frame =
    GBin.scrolled_window
      ~hpolicy:`AUTOMATIC
      ~vpolicy:`ALWAYS
      ~width:270
      ~packing
      ()
  in
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
      "Cancel selection"
      ~key:GdkKeysyms._A
      ~callback:(fun _ -> Timeline.cancel_selection timeline);
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

let prepare filen =
  print_endline ("Preparing " ^ filen);
  let trace = Trace.from_sqlite_file filen in
  Trace.prepare ~verbose:true trace;
  print_endline (filen ^ " prepared");
  ()

let purge filen =
  print_endline ("Purging " ^ filen);
  let trace = Trace.from_sqlite_file filen in
  Trace.purge trace;
  print_endline (filen ^ " purged");
  ()
;;

let reprepare filen =
  print_endline ("Repreparing " ^ filen);
  let trace = Trace.from_sqlite_file filen in
  Trace.purge trace;
  Trace.prepare ~verbose:true trace;
  print_endline (filen ^ " rebuilt");
  ()

let statistics filen =
  print_endline ("Computing statistics for " ^ filen);
  let trace = Trace.from_sqlite_file filen in
  let stats = Trace.statistics trace in
  Format.printf "%a@?"
    Trace.print_stats stats;
  ()

let () =
  let files = ref [] in

  let args =
    [
      "-sqldebug", Arg.Set Sql.debug, "Display SQL queries";
      "-prep", Arg.String prepare, "Build cache tables in file";
      "-purge", Arg.String purge, "Delete cache tables in file";
      "-reprep", Arg.String reprepare, "Rebuild cache tables in file";
      "-stats", Arg.String statistics, "Display statistics for file";
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
