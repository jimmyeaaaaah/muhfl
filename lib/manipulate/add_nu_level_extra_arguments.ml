open Hflmc2_syntax
module Env = Env_no_value
(* open Add_arguments_definition *)
module ADef2 = Add_arguments_definition
module ATuple = Add_arguments_tuple

let extra_argument_name = "uuuuu"
let gen_id1 () =
  let id = Id.gen ATuple.TInt in
  { id with name = extra_argument_name ^ string_of_int id.id}
let extra_parameter_name = "vvvvv"
let gen_id2 () =
  let id = Id.gen ATuple.TInt in
  { id with name = extra_argument_name ^ string_of_int id.id}

let rec conv_ty ty : ATuple.ptype2  =
  match ty with
  | ATuple.TFunc (argtys, bodyty) ->
    let new_argtys =
      if List.exists (fun (argty, _) -> match argty with ATuple.TFunc _ -> true | _ -> false) argtys then
        [(ATuple.TInt, ADef2.dummy_use_flag)]
      else [] in
    let argtys = List.map (fun (ty, f) -> (conv_ty ty, f)) argtys in
    let bodyty = conv_ty bodyty in
    ATuple.TFunc (new_argtys @ argtys, bodyty)
  | TInt | TBool | TVar _ -> ty

(* let eta_expand body =
  let original_ty = ATuple.get_thflz2_type_without_check body in
  let rec go_abs added_args ty = match ty with
  | ATuple.TFunc (argtys, bodyty) ->
    let args = List.map (fun (argty, _) -> Id.gen argty) argtys in
    ATuple.Abs (args, go_abs (args :: added_args) bodyty, ty)
  | TBool  ->
    let new_id = gen_id2 () in
    let rec go_app args body = match args with
      | arg::args ->
        ATuple.App (go_app args body, List.map (fun v -> ATuple.Var v) arg)
      | [] -> body
    in
    let body = go_app added_args body in
    let body = ATuple.Exists (new_id, body) in
    body
  | TVar _ | TInt -> assert false
  in
  let body = go_abs [] original_ty in
  body *)
  
let add_extra_arguments body =
  let rec go body' = match body' with
    | ATuple.Abs (args, body, ty') -> begin
      let args =
        if List.exists (fun arg -> match arg.Id.ty with ATuple.TFunc _ -> true | _ -> false) args then begin
          let new_id = gen_id1 () in
          new_id::args
        end else args in
      let body, new_ids = go body in
      ATuple.Abs (List.map (fun arg -> { arg with Id.ty = conv_ty arg.Id.ty }) args, body, conv_ty ty'), new_ids
    end
    | App (body, args) -> begin
      let body, new_ids =
        let body, new_ids = go body in
        let args, new_ids' = List.map go args |> List.split in
        let new_ids = new_ids @ (List.flatten new_ids') in
        let argtys = List.map ATuple.get_thflz2_type_without_check args in
        if List.exists (fun argty -> match argty with | ATuple.TFunc _ -> true | _ -> false) argtys then begin
          let new_id = gen_id2 () in
          ATuple.App (body, (Arith (Var new_id))::args), (new_id::new_ids)
        end else App (body, args), new_ids in
      match ATuple.get_thflz2_type_without_check body' with
      | TBool ->
        let rec go_exists ids = match ids with
          | x::xs ->  
            ATuple.Exists (x, go_exists xs)
          | [] -> body
        in
        go_exists new_ids, []
      | _ ->
        body, new_ids
    end
    | Bool b -> Bool b, []
    | Var v -> Var { v with ty = conv_ty v.Id.ty }, []
    | Or (p1, p2) ->
      let p1, new_ids = go p1 in
      let p2, new_ids' = go p2 in
      Or (p1, p2), new_ids @ new_ids'
    | And (p1, p2) ->
      let p1, new_ids = go p1 in
      let p2, new_ids' = go p2 in
      And (p1, p2), new_ids @ new_ids'
    | Forall (x, p) ->
      let p, new_ids = go p in
      Forall (x, p), new_ids
    | Exists (x, p) ->
      let p, new_ids = go p in
      Exists (x, p), new_ids
    | Arith a -> Arith a, []
    | Pred (p, l) -> Pred (p, l), []
  in
  go body

let run (hes : 'a Hflz.hes) with_partial_analysis no_temp_files =
  let original_rules = Hflz.merge_entry_rule hes in
  let module PA = Add_arguments_infer_partial_application in
  
  let rules =
    Add_arguments_definition.show_tag_as_separator := true;
    let rules = PA.to_thflzs original_rules in
    let rules =
      if with_partial_analysis then PA.infer_thflz_type rules else PA.set_use_tag rules in
    let () =
      (* print_endline "result (infer partial):";
      print_endline @@
        Hflmc2_util.fmt_string
          (Print_temp.hflz_hes pp_ptype) rules; *)
      (* print_endline "result (full)";
      print_endline @@ show_s_thes_rules rules; *)
      if not no_temp_files then
        ADef2.save_to_file "tmp_t10.txt" @@
          Hflmc2_util.fmt_string
            (ADef2.Print_temp.hflz_hes ADef2.pp_ptype) rules;
      PA.check_thflz_type rules;
      in
    Add_arguments_definition.show_tag_as_separator := false;
    let rules = Add_arguments_tuple.to_thflz2 rules in
    Add_arguments_tuple.check_thflz2_type rules;
    rules in
  (* hoを含む領域に引数を追加。各usageで、 *)
  let rules =
    List.map
      (fun {ATuple.var; body; fix} ->
        let body, args = add_extra_arguments body in
        assert (List.length args = 0);
        {ATuple.var = { var with ty = conv_ty var.ty}; body; fix}
      )
      rules in
  let () =
      if not no_temp_files then
        ADef2.save_to_file "tmp_t11.txt" @@
          Hflmc2_util.fmt_string
            (ATuple.Print_temp.hflz_hes ATuple.pp_ptype2) rules;
      Add_arguments_tuple.check_thflz2_type rules;
      in
  let rules = Add_arguments_tuple.to_hes rules in
  let hes = Hflz.decompose_entry_rule rules in
  Hflz_typecheck.type_check hes;
  hes
