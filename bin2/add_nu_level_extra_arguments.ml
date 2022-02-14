open Core

type show_style = Asis_id | Abbrev_id

let map_file_path path converter =
  let dir, base, ext =
    Stdlib.Filename.dirname path,
    Stdlib.Filename.remove_extension (Stdlib.Filename.basename path),
    Stdlib.Filename.extension path in
  let dir, base, ext = converter (dir, base, ext) in
  Stdlib.Filename.concat dir (base ^ ext)

let main filepath =
  Logs.set_level (Some Logs.Debug);
  let hes = Muapprox.parse filepath in
  let hes = Muapprox.add_nu_level_extra_arguments hes true false in
  let path2 = map_file_path filepath (fun (a, b, c) -> (a, b ^ "_add_nu_level_extra_arguments", c)) in
  ignore @@ Muapprox.Manipulate.Print_syntax.MachineReadable.save_hes_to_file ~file:path2 ~without_id:true true hes;
  print_endline @@ "Saved as " ^ path2

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
