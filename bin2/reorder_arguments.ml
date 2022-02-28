open Core

let main filepath =
  Logs.set_level (Some Logs.Debug);
  let hes_original = Muapprox.parse filepath in
  let () =
    let path2' = filepath ^ "_reorder_orig.in" in
    ignore @@ Muapprox.Manipulate.Print_syntax.MachineReadable.save_hes_to_file ~file:path2' ~without_id:true true hes_original;
    print_endline @@ "Before Reordered: saved in " ^ path2' in
  let hes = Muapprox.reorder_arguments hes_original true false in
  let path2 = filepath ^ "_reorder.in" in
  let () =
    ignore @@ Muapprox.Manipulate.Print_syntax.MachineReadable.save_hes_to_file ~file:path2 ~without_id:true true hes;
    print_endline @@ "Reordered: saved in " ^ path2 in
  ()
  
let command =
  Command.basic
    ~summary:""
    Command.Let_syntax.(
      let%map_open
          filepath = anon ("filepath" %: string)
      in
      (fun () -> main filepath)
    )

let () = Command.run command
