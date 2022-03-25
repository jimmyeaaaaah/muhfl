

module ConvertLib = struct
  let trans_op op = match op with
    | Muapprox.Syntax.Arith.Add -> "+"
    | Sub -> "-"
    | Mult -> "*"
    | Div | Mod -> failwith "trans_op: unsupported operator"
  
  let trans_pred op = match op with
    | Muapprox.Syntax.Formula.Eq -> "="
    | Le -> "<="
    | Ge -> ">="
    | Lt -> "<"
    | Gt -> ">"
    | Neq -> failwith "trans_pred: Neq"

  let rec trans_expr expr =
    match expr with
    | Muapprox.Syntax.Raw_hflz.Bool b -> string_of_bool b
    | Var v -> v
    | Or (e1, e2)  -> "(or "  ^ trans_expr e1 ^ " " ^ trans_expr e2 ^ ")"
    | And (e1, e2) -> "(and " ^ trans_expr e1 ^ " " ^ trans_expr e2 ^ ")"
    | App (e1, e2) -> "(" ^ trans_expr e1 ^ " " ^ trans_expr e2 ^ ")"
    | Int i -> string_of_int i
    | Op   (op, es) -> "(" ^ trans_op   op ^ " " ^ (List.map trans_expr es |> String.concat " ") ^ ")"
    | Pred (pr, es) -> "(" ^ trans_pred pr ^ " " ^ (List.map trans_expr es |> String.concat " ") ^ ")"
    | Forall (x, e) -> "(forall (" ^ x ^ ") " ^ trans_expr e ^ ")"
    | Exists (x, e) -> "(exists (" ^ x ^ ") " ^ trans_expr e ^ ")"
    | Not (e)  -> "(not " ^ trans_expr e ^ ")"
    | Abs _ -> failwith "unsupported (Abs)"
    
end

open ConvertLib
(* open Core *)

let map_file_path path converter =
  let dir, base, ext =
    Stdlib.Filename.dirname path,
    Stdlib.Filename.remove_extension (Stdlib.Filename.basename path),
    Stdlib.Filename.extension path in
  let dir, base, ext = converter (dir, base, ext) in
  Stdlib.Filename.concat dir (base ^ ext)

let parse_to_raw file =
  Core.In_channel.with_file file ~f:begin fun ch ->
    let lexbuf = Lexing.from_channel ch in
    lexbuf.lex_start_p <- { lexbuf.lex_start_p with pos_fname = file };
    lexbuf.lex_curr_p  <- { lexbuf.lex_curr_p  with pos_fname = file };
    lexbuf
    |> Muapprox.Syntax.Parser.main
  end

let show_as_prefix_notation {Muapprox.Syntax.Raw_hflz.body; _} =
  print_endline @@ trans_expr body

let main file =
  let hes, _ = parse_to_raw file in
  List.iter show_as_prefix_notation hes
  
let command =
  Core.Command.basic
    ~summary:"Convert to prefix notation"
    Core.Command.Let_syntax.(
      let%map_open
          file = anon ("file" %: string)
      in
      (fun () -> main file)
    )

let () = Core.Command.run command
