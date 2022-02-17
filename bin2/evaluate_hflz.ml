open Core

(* let map_file_path path converter =
  let dir, base, ext =
    Stdlib.Filename.dirname path,
    Stdlib.Filename.remove_extension (Stdlib.Filename.basename path),
    Stdlib.Filename.extension path in
  let dir, base, ext = converter (dir, base, ext) in
  Stdlib.Filename.concat dir (base ^ ext) *)

let main path1 max_expansion max_expansion_show shortcircuit try_values =
  let max_expansion = match max_expansion with Some i -> i | None -> 3 in
  let max_expansion_show = match max_expansion_show with Some i -> i | None -> 2 in
  let try_values = match try_values with None -> [1; 2] | Some try_values -> String.split try_values ~on:',' |> List.map ~f:int_of_string in
  Muapprox.Manipulate.Evaluate_hflz.max_expansion := max_expansion;
  Muapprox.Manipulate.Evaluate_hflz.max_expansion_show := max_expansion_show;
  Muapprox.Manipulate.Evaluate_hflz.shortcircuit := shortcircuit;
  let phi1 = Muapprox.parse path1 in
  let phi1 = Muapprox.Manipulate.Evaluate_hflz.evaluate_hes phi1 try_values in
  ignore @@ phi1


let command =
  Command.basic
    ~summary:"Evaluate HFLz"
    Command.Let_syntax.(
      let%map_open
          path = anon ("path1" %: string)
      and max_expansion = flag "--max-expansion" (optional int) ~doc:""
      and max_expansion_show = flag "--max-expansion-show" (optional int) ~doc:""
      and shortcircuit = flag "--shortcircuit" no_arg ~doc:""
      and try_values = flag "--try-values" (optional string) ~doc:""
      in
      (fun () -> main path max_expansion max_expansion_show shortcircuit try_values)
    )

let () = Command.run command
