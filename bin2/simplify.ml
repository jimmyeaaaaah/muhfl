open Core

type show_style = Asis_id | Abbrev_id

let map_file_path path converter =
  let dir, base, ext =
    Stdlib.Filename.dirname path,
    Stdlib.Filename.remove_extension (Stdlib.Filename.basename path),
    Stdlib.Filename.extension path in
  let dir, base, ext = converter (dir, base, ext) in
  Stdlib.Filename.concat dir (base ^ ext)

let main filepath optimization agg show_style trivial_only_agg output_cp =
  Logs.set_level (Some Logs.Debug);
  let hes = Muapprox.parse filepath in
  let () =
    if output_cp then
      (let hes = Muapprox.constant_propagation hes in
      let path2 = filepath ^ "_cp.in" in
      ignore @@ Muapprox.Manipulate.Print_syntax.MachineReadable.save_hes_to_file ~file:path2 ~without_id:(Stdlib.(=) show_style Abbrev_id) true hes;
      print_endline @@ "Constant Propagation: saved in " ^ path2) else ()
  in
  let () =
    let hes = Muapprox.simplify_if_condition "z3" hes in
    let path2 = filepath ^ "_simif.in" in
    ignore @@ Muapprox.Manipulate.Print_syntax.MachineReadable.save_hes_to_file ~file:path2 ~without_id:(Stdlib.(=) show_style Abbrev_id) true hes;
    print_endline @@ "Simplify if conditions: saved in " ^ path2
  in
  let hes =
    if optimization then Muapprox.eliminate_unused_argument hes else hes in
  let hes = Muapprox.Manipulate.Hes_optimizer.simplify_all hes in
  let hes =
    if agg then Manipulate.Hes_optimizer.simplify_agg trivial_only_agg hes else hes in
  let hes =
    match show_style with
    | Abbrev_id -> Muapprox.abbrev_variable_names hes
    | Asis_id -> hes in
  let path2 = map_file_path filepath (fun (a, b, c) -> (a, b ^ "_simplified", c)) in
  ignore @@ Muapprox.Manipulate.Print_syntax.MachineReadable.save_hes_to_file ~file:path2 ~without_id:(Stdlib.(=) show_style Abbrev_id) true hes;
  print_endline @@ "Simplified to " ^ path2

let read_show_style = 
  Command.Arg_type.create (fun style ->
      match style with
      | "asis" -> Asis_id
      | "abbrev" -> Abbrev_id
      | _ -> failwith "style should be one of raw, escape, or abbrev")
  
let command =
  Command.basic
    ~summary:"Simplify HES formula"
    Command.Let_syntax.(
      let%map_open
          filepath = anon ("filepath" %: string)
      and optimization = flag "--optimization" no_arg ~doc:"eliminatate unused arguments"
      and agg = flag "--agg" no_arg ~doc:"aggressive inlining"
      and trivial_only_agg = flag "--trivial-only-agg" no_arg ~doc:""
      and output_cp = flag "--output-cp" no_arg ~doc:""
      and show_style =
        flag "--show-style" (optional_with_default Asis_id read_show_style) ~doc:"output id without escaping (for debug)"
      in
      (fun () -> main filepath optimization agg show_style trivial_only_agg output_cp)
    )

let () = Command.run command
