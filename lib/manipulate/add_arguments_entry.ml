open Hflmc2_syntax
module Env = Env_no_value
open Add_arguments_definition

let eliminate_unused_universal_quantifiers_for_extra_arguments body =
  let s_len = String.length Add_arguments_adding.extra_arg_name in
  let rec go body = match body with
    | Hflz.Forall (x, body) -> begin
      let result =
        if String.length x.Id.name >= s_len
            && String.sub x.Id.name 0 s_len = Add_arguments_adding.extra_arg_name then begin
          match Hflz_util.decompose_ors x body with
          | Some (_simple_ors, body) ->
            Some (go body)
          | None -> None
        end else None in
      match result with
      | Some f -> f
      | None -> Hflz.Forall (x, go body)
    end
    | Var v -> Var v
    | Bool b -> Bool b
    | Or (p1, p2) -> Or (go p1, go p2)
    | And (p1, p2) -> And (go p1, go p2)
    | Abs (x, body) -> Abs (x, go body)
    | Exists (x, body) -> Exists (x, go body)
    | App (p1, p2) -> App (go p1, go p2)
    | Arith a -> Arith a
    | Pred (p, b) -> Pred (p, b)
  in
  go body

let infer with_partial_analysis with_usage_analysis (hes : 'a Hflz.hes) add_arg_coe1 add_arg_coe2 no_temp_files (do_not_use_inner_ty : bool) =
  let original_rules = Hflz.merge_entry_rule hes in
  let module PA = Add_arguments_infer_partial_application in
  
  let rules =
    Add_arguments_definition.show_tag_as_separator := true;
    let rules = PA.to_thflzs original_rules in
    (* print_endline "to_thflz";
    print_endline @@ show_s_thes_rules rules;
    print_endline "to_thflz (simple)";
    print_endline @@
      Hflmc2_util.fmt_string
        (Print_temp.hflz_hes pp_ptype) rules; *)
    
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
        save_to_file "tmp_t7.txt" @@
          Hflmc2_util.fmt_string
            (Print_temp.hflz_hes pp_ptype) rules;
      PA.check_thflz_type rules;
      in
    Add_arguments_definition.show_tag_as_separator := false;
    rules in
  
  (* let rec_flags = construct_recursion_flags original_rules in *)
  let outer_mu_funcs = Hflz_manipulate.get_outer_mu_funcs original_rules in
  
  let rules =
    let rules = Add_arguments_tuple.to_thflz2 rules in
    Add_arguments_tuple.check_thflz2_type rules;
    let rules =
      if with_usage_analysis then Add_arguments_infer_usage.infer_thflz_type rules outer_mu_funcs do_not_use_inner_ty else Add_arguments_infer_usage.set_use_tag rules in
    let () =
      (* print_endline "result:";
      print_endline @@
        Hflmc2_util.fmt_string
          (Print_temp.hflz_hes_in_out pp_ptype2) rules; *)
      if not no_temp_files then
        Add_arguments_definition.save_to_file "tmp_t8.txt" @@
          Hflmc2_util.fmt_string
            (Add_arguments_tuple.Print_temp.hflz_hes_in_out Add_arguments_tuple.pp_ptype2) rules;
    in
    rules in
  
  let rules, id_type_map, id_ho_map =
    Add_arguments_adding.add_params add_arg_coe1 add_arg_coe2 outer_mu_funcs rules do_not_use_inner_ty in
  let rules = Add_arguments_adding.to_hes rules in
  
  let rules =
    List.map
      (fun {Hflz.var; body; fix} ->
        {Hflz.var; body = eliminate_unused_universal_quantifiers_for_extra_arguments body; fix}
      ) rules in
  let hes = Hflz.decompose_entry_rule rules in
  let hes = Hflz_typecheck.set_variable_ty hes in  
  Hflz_typecheck.type_check hes;
  hes, id_type_map, id_ho_map
