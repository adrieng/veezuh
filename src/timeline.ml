open Cairo

let pi2 = 8. *. atan 1.

let draw ~width ~height cr =
  Printf.printf "width: %f, height: %f\n" width height;
  let r = 0.25 *. width in
  set_source_rgba cr 0. 1. 0. 0.5;
  arc cr (0.5 *. width) (0.35 *. height) r 0. pi2;
  fill cr;
  set_source_rgba cr 1. 0. 0. 0.5;
  arc cr (0.35 *. width) (0.65 *. height) r 0. pi2;
  fill cr;
  set_source_rgba cr 0. 0. 1. 0.5;
  arc cr (0.65 *. width) (0.65 *. height) r 0. pi2;
  fill cr;
;;

class timeline ~packing trace p =
  let box = GPack.hbox ~packing () in
  let lbl =
    GMisc.label ~text:(Printf.sprintf "Processor %d" p) ~packing:box#add () in
  let da = GMisc.drawing_area ~packing:box#add () in
  object (self)
    inherit GObj.widget box#as_widget

    method expose () =
      ()

    initializer
      Printf.printf "Timeline %d initialized.\n" p;
      ()
  end

class timelines ~packing trace =
  let pcount = Trace.number_of_processors trace in

  (* let layout = GPack.layout ~packing () in *)

  let lines = Array.init pcount (fun p -> new timeline packing trace p) in
  object (self)
    (* inherit GObj.widget layout#as_widget *)

    method expose () =
      Array.iter (fun tl -> tl#expose ()) lines;

    initializer
      (* let callback _ = self#expose (); false in *)
      (* ignore @@ layout#event#connect#expose ~callback; *)
      Printf.printf "Timelines initialized.\n"
  end
