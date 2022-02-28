open Hflmc2_syntax
module Env = Env_no_value
module ADef2 = Add_arguments_definition
module ATuple = Add_arguments_tuple

let should_be_first ty =
  match ty with
  | ATuple.TInt -> true
  | ATuple.TVar _ -> assert false
  | _ -> false

let rec conv_ty ty : ATuple.ptype2 =
  let reorder_types tys =
    let (int_tys, pred_tys) =
      List.partition (fun (ty, _) -> should_be_first ty) tys in
    int_tys @ pred_tys in
  match ty with
  | ATuple.TFunc (argtys, bodyty) ->
    let argtys =
      argtys
      |> reorder_types
      |> List.map (fun (ty, f) -> (conv_ty ty, f)) in
    let bodyty = conv_ty bodyty in
    ATuple.TFunc (argtys, bodyty)
  | TVar _ -> assert false
  | TInt | TBool -> ty

let reorder_hflz body =
  let reorder_args args =
    let (int_args, pred_args) =
      List.partition (fun arg -> should_be_first arg.Id.ty) args in
    int_args @ pred_args in
  let reorder_apps apps =
    let (int_apps, pred_apps) =
      List.partition
        (fun app -> should_be_first (ATuple.get_thflz2_type_without_check app))
        apps in
    int_apps @ pred_apps in
  let rec go body = match body with
    | ATuple.Abs (args, body, ty') -> begin
      let args =
        args
        |> reorder_args
        |> List.map (fun arg -> { arg with Id.ty = conv_ty arg.Id.ty }) in
      let body = go body in
      ATuple.Abs (args, body, conv_ty ty')
    end
    | App (body, args) -> begin
      let args =
        args
        |> reorder_apps
        |> List.map go in
      let body = go body in
      App (body, args)
    end
    | Bool b -> Bool b
    | Var v -> Var { v with ty = conv_ty v.ty }
    | Or (p1, p2) ->
      Or (go p1, go p2)
    | And (p1, p2) ->
      And (go p1, go p2)
    | Forall (x, p) ->
      Forall (x, go p)
    | Exists (x, p) ->
      Exists (x, go p)
    | Arith a -> Arith a
    | Pred (p, l) -> Pred (p, l)
  in
  go body

let run (hes : 'a Hflz.hes) with_partial_analysis no_temp_files =
  let original_rules = Hflz.merge_entry_rule hes in
  let module PA = Add_arguments_infer_partial_application in
  
  (* 連続適用で引数を分割 *)
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
        ADef2.save_to_file "tmp_reorder1.txt" @@
          Hflmc2_util.fmt_string
            (ADef2.Print_temp.hflz_hes ADef2.pp_ptype) rules;
      PA.check_thflz_type rules;
      in
    Add_arguments_definition.show_tag_as_separator := false;
    let rules = Add_arguments_tuple.to_thflz2 rules in
    Add_arguments_tuple.check_thflz2_type rules;
    rules in
  
  (* 引数の並びを変える *)
  let rules =
    List.map
      (fun {ATuple.var; body; fix} ->
        let body = reorder_hflz body in
        {ATuple.var = { var with ty = conv_ty var.ty }; body; fix}
      )
      rules in
  let () =
      if not no_temp_files then
        ADef2.save_to_file "tmp_reorder2.txt" @@
          Hflmc2_util.fmt_string
            (ATuple.Print_temp.hflz_hes ATuple.pp_ptype2) rules;
      Add_arguments_tuple.check_thflz2_type rules;
      in
  let rules = Add_arguments_tuple.to_hes rules in
  let hes = Hflz.decompose_entry_rule rules in
  hes
