open Hflmc2_syntax
(* 
module Hesutil = struct
  let iter_app (f : Hflz.t -> unit) phi = 
    match phi with
    | 
end *)

let log_src = Logs.Src.create "Optimizer"
module Log = (val Logs.src_log @@ log_src)

let log_string = Hflz_util.log_string Log.info

let show_hes s hes =
  print_endline s;
  print_endline
    (hes
    |> Hflz.merge_entry_rule
    |> Print_syntax.show_hes ~readable:true);
  hes

let log_hes s hes =
  log_string s;
  log_string (hes
    |> Hflz.merge_entry_rule
    |> Print_syntax.show_hes ~readable:true);
  let () =
    match Logs.level () with
    | Some (Logs.Info) | Some (Logs.Debug) -> begin
      let hes' = Abbrev_variable_numbers.abbrev_variable_numbers_hes hes in
      let path = s ^ ".tmp.in" in
      ignore @@
        try
          Print_syntax.MachineReadable.save_hes_to_file ~file:path  ~without_id:true true hes'
        with _ -> log_string @@ "Error when saving the file \"" ^ path ^ "\""; ""
    end
    | _ -> () in
  hes
  
let get_pvar_called_counts hes =
  let preds, graph = Hflz_util.get_dependency_graph hes in
  let graph = Mygraph.reverse_edges graph in
  preds
  |> List.map (fun (i, _var) -> List.length @@ Mygraph.get_next_nids i graph)  

(*
  inline expansion

  if a predicate P1 is called from only one predicate P2
  and depth(P1) > depth(P2)
  then P1 is eliminated by inline expansion
  (the reason of this is trivial when you think the hes as a plain HFLz formula)
*)
module InlineExpansion (* : sig
  val optimize: 'a Hflz.hes -> 'a Hflz.hes
end*) = struct
(* 
  (* 変数の出現を置換 *)
  let replace_var_occurences
      (subst : 'ty Id.t -> 'ty Hflz.t option)
      (hfl : 'ty Hflz.t): 'ty Hflz.t =
    (* TODO: IDのeqが正しく判定される？ *)
    let rec go : 'ty Hflz.t -> 'ty Hflz.t = function
      | Var   id -> begin
        match subst (id) with
        | None -> Var id
        | Some term -> Trans.Subst.Hflz.rename_binding_if_necessary b_env term
      end
      | Bool   b -> Bool b
      | Or (f1, f2) -> Or (go f1, go f2)
      | And (f1, f2) -> And (go f1, go f2)
      | Abs (v, f1) -> Abs (v, go f1)
      | Forall (v, f1) -> Forall (v, go f1)
      | Exists (v, f1) -> Exists (v, go f1)
      | App (f1, f2) -> App (go f1, go f2)
      | Arith t -> Arith t
      | Pred (p, t) -> Pred (p, t)
    in
    (* predicateはboolean以外を返すことは無い。arithmeticの中にhfl predicateは現れない *)
    go hfl *)
    
  let optimize (org_hes : 'a Hflz.hes) =
    let hes_list = (Hflz.merge_entry_rule org_hes) in
    let n = List.length hes_list in
    let pvar_to_id = List.mapi (fun i {Hflz.var;_} -> (var, i)) hes_list in
    let called_counts = get_pvar_called_counts hes_list in
    let expanded = Array.make n false in
    let hes = Array.of_list hes_list in
    List.rev hes_list
    |> List.iteri
      (fun i ({Hflz.body;_ } as rule) ->
        let func_id = n - i - 1 in
        let subst_env =
          pvar_to_id
          |> List.filter_map (fun (v', func_id') ->
            if List.nth called_counts func_id' = 1 && func_id' > func_id then
              Some (v', hes.(func_id').body)
            else None
          )
          |> IdMap.of_list
          in
        let body =
          Trans.Subst.Hflz.hflz ~callback:(fun v _ ->
            match List.find_opt (fun (v', _) -> Id.eq v v') pvar_to_id with
            | Some (_, func_id') -> 
              expanded.(func_id') <- true
            | None -> assert false
          )
          subst_env
          body in
        hes.(func_id) <- { rule with body = Trans.Simplify.hflz body }
      );
    Array.to_list hes
    |> List.mapi (fun i r -> (i, r))
    |> List.filter (fun (id, _) -> not expanded.(id))
    |> List.map (fun (_, r) -> r)
    |> Hflz.decompose_entry_rule

  let get_recursivity (rules : 'a Type.ty Hflz.hes_rule list) =
    let preds, graph = Hflz_util.get_dependency_graph rules in
    List.map
      (fun (i, pred) ->
        let reachables = Mygraph.reachable_nodes_from ~start_is_reachable_initially:false i graph in
        match List.find_opt (fun r -> r = i) reachables with
        | Some _ -> (pred, true)
        | None -> (pred, false)
      )
      preds

  let has_references (_, rules) {Hflz.body; _} =
    let rec go phi =
      match phi with
      | Hflz.Var v -> List.exists (fun {Hflz.var; _} -> Id.eq var v) rules
      | Pred _ | Arith _ | Bool _ -> false
      | Or (p1, p2) -> go p1 || go p2
      | And (p1, p2) -> go p1 || go p2
      | App (p1, p2) -> go p1 || go p2
      | Abs (_, p) -> go p
      | Forall (_, p) -> go p
      | Exists (_, p) -> go p
    in
    go body
    
  let inline_non_recursive_variables (no_ref_only : bool) (trivial_only : bool) (org_hes : 'a Hflz.hes) =
    let org_rules = Hflz.merge_entry_rule org_hes in
    let rec_flags = get_recursivity org_rules in
    log_string @@ Hflmc2_util.show_list (fun (id, f) -> id.Id.name ^ ": " ^ string_of_bool f) rec_flags;
    let to_inlinings =
      rec_flags
      |> List.tl
      |> List.filter
        (fun (p, is_rec) ->
          not is_rec &&
          (
            not no_ref_only ||
            not @@ has_references org_hes (List.find (fun {Hflz.var; _} -> Id.eq var p) org_rules)
          ) &&
          (
            not trivial_only ||
            (
              let {Hflz.body; _} = List.find (fun {Hflz.var; _} -> Id.eq var p) org_rules in
              body = Bool true || body = Bool false
            )
          )
        )
      |> List.map (fun (p, _) -> p)
    in
    let rules =
      List.fold_left
        (fun rules to_inlining ->
          let {Hflz.var; body; _} = List.find (fun {Hflz.var; _} -> Id.eq var to_inlining) rules in
          
          let subst_env = IdMap.of_list [(var, body)] in
          
          rules
          |> List.filter (fun {Hflz.var; _} -> not @@ Id.eq var to_inlining) 
          |> List.map
            (fun {Hflz.var; body; fix} ->
              let body = Trans.Subst.Hflz.hflz subst_env body in
              let body = Trans.Simplify.hflz body in
              {Hflz.var; body; fix}
            )
        )
        org_rules
        to_inlinings in
    Hflz.decompose_entry_rule rules
end

let simple_partial_evaluate_hfl phi =
  let rec go phi = match phi with
    | Hflz.Arith a -> Hflz.Arith (Arith.simple_partial_evaluate a)
    | Pred (p, xs) -> Pred (p, List.map Arith.simple_partial_evaluate xs)
    | Var _ | Bool _ -> phi
    | Or (p1, p2) -> Or (go p1, go p2)
    | And(p1, p2) -> And(go p1, go p2)
    | Abs(x, p) -> Abs(x, go p)
    | Forall(x, p) -> Forall(x, go p)
    | Exists(x, p) -> Exists(x, go p)
    | App(p1, p2) -> App(go p1, go p2)
  in
  go phi

let simple_partial_evaluate_hes hes =
  Hflz.merge_entry_rule hes |>
  List.map (fun rule -> { rule with Hflz.body = simple_partial_evaluate_hfl rule.Hflz.body }) |>
  Hflz.decompose_entry_rule

let evaluate_trivial_boolean phi =
  let rec go phi = match phi with
    | Hflz.Pred (p, xs) -> begin
      match Formula.simplify_pred p xs with
      | Some b -> Hflz.Bool b
      | None -> phi
    end
    | And (p1, p2) -> begin
      match go p1, go p2 with
      | Bool true , Bool true -> Bool true
      | Bool false, _ -> Bool false
      | _, Bool false -> Bool false
      | Bool true, p -> p
      | p, Bool true -> p
      | p1, p2 -> And (p1, p2)
    end
    | Or (p1, p2) -> begin
      match go p1, go p2 with
      | Bool false, Bool false -> Bool false
      | Bool true , _ -> Bool true
      | _ , Bool true -> Bool true
      | Bool false , p -> p
      | p , Bool false -> p
      | p1, p2 -> Or (p1, p2)
    end
    | Forall (x, p) -> Forall (x, go p)
    | Exists (x, p) -> Exists(x, go p)
    | Abs (x, p) -> Abs( x, go p)
    | App (p1, p2) -> App (go p1, go p2)
    | Arith _ -> phi
    | Bool _ -> phi
    | Var _ -> phi in
  go phi

let evaluate_trivial_boolean hes =
  Hflz.merge_entry_rule hes |>
  List.map (fun rule -> { rule with Hflz.body = evaluate_trivial_boolean rule.Hflz.body }) |>
  Hflz.decompose_entry_rule
  
let beta_hes (entry, rules) =
  Hflz_util.beta IdMap.empty entry |> snd,
  List.map
    (fun rule ->
      let _, body = Hflz_util.beta IdMap.empty rule.Hflz.body in
      { rule with Hflz.body = body }
    )
    rules

let evaluate_trivial_fixpoints (entry, rules) =
  let rules =
    List.map
      (fun ({Hflz.var; body; fix} as rule) ->
        if body = Var var then begin
          let body =
            match fix with
            | Least -> Hflz.Bool false
            | Greatest -> Bool true in
          {rule with body}
        end else
          rule
      )
      rules in
  (entry, rules)

(* 2つ、1つで下、1つで上、1つで中、betaされる *)
(* 1つの述語の中で2回参照される *)
let%expect_test "InlineExpansition.optimize" =
  let open Type in
  let open Hflz in
  let ty2 = TyArrow (id_n 202 TyInt, TyBool ()) in
  let pvars = [
      id_n 000 (TyArrow (id_n 001 (TyInt), TyBool ()));
    id_n 100 (TyArrow (id_n 101 TyInt, TyBool ()));
    id_n 200 (TyArrow (id_n 201 (TySigma ty2), TyBool ()));
    id_n 300 (TyArrow (id_n 301 TyInt, TyBool ()));
    id_n 400 (TyArrow (id_n 401 TyInt, TyBool ()));
    (* id_n 500 (TyArrow (id_n 501 (TySigma (TyBool ())), TyBool ()));
    id_n 600 (TyArrow (id_n 601 (TySigma (TyBool ())), TyBool ())); *)
  ] in
  let nth n = List.nth pvars n in
  (* 
    X1 = \(x101:int). X2 X1 \/ X2 X1;
    X2 = \(x201:int->bool). x201 2 /\ (X4 3);
    X3 = \(x301:int). X4 x301;
    X4 = \(x401:int). x401 = 5 /\ X3 6;
    
    X1 ... x (同じ階層から参照),
    X2 ... o (同じ述語から2回参照), 
    X3 ... x (下から上に参照),
    X4 ... x (2回参照), 
    
    expected result
    X1 = \(x101:int). (\(x201:int->bool). x201 2 /\ (X4 3)) X1 \/ (\(x201:int->bool). x201 2 /\ (X4 3)) X1;
    X3 = \(x301:int). X4 x301;
    X4 = \(x401:int). x401 = 5 /\ X3 6;
    
    
    X1 = \(x101:int). (X1 2 /\ X4 3) \/ (X1 2 /\ X4 3);
    X3 = \(x301:int). X4 x301;
    X4 = \(x401:int). x401 = 5 /\ X3 6;
    
   *)
  let org = [
    {
      fix = Fixpoint.Greatest;
      var = nth 1;
      body = Abs (id_n 101 TyInt, Or (App (Var (nth 2), Var (nth 1)), App (Var (nth 2), Var (nth 1))));
    };{
      fix = Fixpoint.Greatest;
      var = nth 2;
      body = Abs (id_n 201 (TySigma ty2), And (App (Var (id_n 201 ty2), Arith (Int 2)), App (Var (nth 4), Arith (Int 3))));
    };{
      fix = Fixpoint.Greatest;
      var = nth 3;
      body = Abs (id_n 301 TyInt, App (Var (nth 4), Arith (Var (id_n 301 `Int))))
    };{
      fix = Fixpoint.Greatest;
      var = nth 4;
      body = Abs (id_n 401 TyInt, And (Pred (Eq, [Var (id_n 401 `Int); Int 5]), App (Var (nth 3), Arith (Int 6))))
    }] in
  Hflz_typecheck.type_check (Bool true, org);
  ignore [%expect.output];
  print_endline "OK";
  [%expect {| OK |}];
  print_endline @@ Print_syntax.show_hes org;
  [%expect {|
    {fix: Fixpoint.Greatest
    var: (x_100100 : int -> bool)
    body: λx_101101:int.x_200200 x_100100 || x_200200 x_100100}
    {fix: Fixpoint.Greatest
    var: (x_200200 : (int -> bool) -> bool)
    body: λx_201201:(int -> bool).x_201201 2 && x_400400 3}
    {fix: Fixpoint.Greatest
    var: (x_300300 : int -> bool)
    body: λx_301301:int.x_400400 x_301301}
    {fix: Fixpoint.Greatest
    var: (x_400400 : int -> bool)
    body: λx_401401:int.x_401401 = 5 && x_300300 6} |}];
  let hes = InlineExpansion.optimize (Bool true, org) in
  ignore [%expect.output];
  (* ignore [%expect.output]; *)
  Hflz_typecheck.type_check (Bool true, org);
  ignore [%expect.output];
  print_endline "OK";
  [%expect {| OK |}];
  print_endline @@ Print_syntax.show_hes (snd hes);
  (* print_endline @@ show_hes hes; *)
  (* print_endline @@ Util.fmt_string (Print_syntax.hflz_hes_rule Print_syntax.simple_ty_) rule; *)
  [%expect {|
    {fix: Fixpoint.Greatest
    var: (x_100100 : int -> bool)
    body: λx_101101:int.x_100100 2 && x_400400 3 || x_100100 2 && x_400400 3}
    {fix: Fixpoint.Greatest
    var: (x_300300 : int -> bool)
    body: λx_301301:int.x_400400 x_301301}
    {fix: Fixpoint.Greatest
    var: (x_400400 : int -> bool)
    body: λx_401401:int.x_401401 = 5 && x_300300 6} |}]

let eliminate_unreachable_predicates (hes : 'a Hflz.hes) : 'a Hflz.hes =
  let rules = Hflz.merge_entry_rule hes in
  let _, rgraph = Hflz_util.get_dependency_graph rules in
  let reachables = Mygraph.reachable_nodes_from ~start_is_reachable_initially:true 0 rgraph in
  let rules = 
    List.mapi (fun i r -> r, (List.find_all ((=)i) reachables <> [])) rules
    |> List.filter_map (fun (r, b) -> if b then Some r else None) in
  Hflz.decompose_entry_rule rules

let inline_bottom_sub hes =
  let inlined = ref false in
  let get_free_preds body =
    Hflz.fvs body
    |> IdSet.filter ~f:(fun p -> Id.is_pred_name p.name) in
  let rules = Hflz.merge_entry_rule hes in
  (* 自分のレベル以下のpredicateが出現しないpredicateを一番下のレベルに移動する *)
  let (_, (result_rec, result_low)) =
    List.fold_left
      (fun (lower_preds, (result_rec, result_low)) rule ->
        let {Hflz.var; fix = _fix; body} = rule in
        let lower_preds = IdSet.add lower_preds var in
        let free_predicates = get_free_preds body in
        let (r1, r2) =
          if IdSet.is_empty (IdSet.inter free_predicates lower_preds) then
            result_rec, (rule::result_low)
          else
            (rule::result_rec), result_low in
        (lower_preds, (r1, r2))
      )
      (IdSet.empty, ([], []))
      (List.rev rules) in
  log_string @@ "rules: " ^ Print_syntax.show_hes ~readable:true rules;
  log_string @@ "result_rec: " ^ Print_syntax.show_hes ~readable:true result_rec;
  log_string @@ "result_low: " ^ Print_syntax.show_hes ~readable:true result_low;
  if List.exists (fun {Hflz.var; _} -> var.name = Hflz.dummy_entry_name) result_low then
    Hflz.decompose_entry_rule result_low, false
  else begin
    let results = result_rec @ result_low in
    let rec inline_sub rules =
      match List.rev rules with
      | [] -> assert false
      | [_] -> rules
      | {Hflz.var; fix=_fix; body}::rem_rules -> begin
        let rem_rules = List.rev rem_rules in
        let free_predicates = get_free_preds body in
        if IdSet.exists free_predicates ~f:(fun p -> Id.eq p var) then begin
          rules
        end else begin
          inlined := true;
          (* inlining in rules *)
          let rem_rules =
            List.map
              (fun rule ->
                let body =
                  Trans.Subst.Hflz.hflz
                    (IdMap.singleton var body)
                    rule.Hflz.body in
                let body = Trans.Simplify.hflz body in
                { rule with body }
              )
              rem_rules in
          inline_sub rem_rules
        end
      end
    in
    let results = inline_sub results in
    Hflz.decompose_entry_rule results |> log_hes "inline_bottom_sub(after):", !inlined
  end

let rec inline_bottom hes =
  let hes, inlined = inline_bottom_sub hes in
  if inlined then
    inline_bottom hes
  else
    hes

(* (∃x. (not p(x) \/ phi1) /\ (    p(x) \/ phi2)) <=> (phi1 \/ phi2) *)
(* (∃x. (    p(x) /\ phi1) \/ (not p(x) /\ phi2)) <=> (phi1 \/ phi2) *)
(* (∀x. (not p(x) \/ phi1) /\ (    p(x) \/ phi2)) <=> (phi1 /\ phi2) *)
(* (∀x. (    p(x) /\ phi1) \/ (not p(x) /\ phi2)) <=> (phi1 /\ phi2) *)
let simplify_non_deterministic_branch_sub phi =
  let get_both_branch x p11 p12 p21 p22 =
    (* In order to correctly optimize, pred p must satisfy ∃x.p(x)=∃x.¬p(x)=True. As under-approximation, we target primitive predicates without any logical connectives *)
    let pred1, phi1 =
      match p11 with
      | Hflz.Pred _ -> p11, p12
      | _ -> p12, p11 in
    let pred2, phi2 =
      match p21 with
      | Hflz.Pred _ -> p21, p22
      | _ -> p22, p21 in
    if (IdSet.equal (IdSet.singleton {x with ty=`Int}) (Hflz.fvs pred1))
        && not (IdSet.exists ~f:(fun x' -> Id.eq x x') (Hflz.fvs phi1))
        && not (IdSet.exists ~f:(fun x' -> Id.eq x x') (Hflz.fvs phi2))
        && Hflz.is_negation_of pred1 pred2 then
      Some (phi1, phi2)
    else None
  in
  let rec go phi = match phi with
    | Hflz.Forall (x, phi) -> begin
      match phi with
      | And (Or (p11, p12), Or (p21, p22))
      | Or (And (p11, p12), And (p21, p22)) -> begin
        match get_both_branch x p11 p12 p21 p22 with
        | None -> Hflz.Forall (x, go phi)
        | Some (p1, p2) ->
          Hflz.And (go p1, go p2)
      end
      | _ -> Forall (x, go phi)
    end
    | Exists (x, phi) -> begin
      match phi with
      | And (Or (p11, p12), Or (p21, p22))
      | Or (And (p11, p12), And (p21, p22)) -> begin
        match get_both_branch x p11 p12 p21 p22 with
        | None -> Hflz.Exists (x, go phi)
        | Some (p1, p2) ->
          Hflz.Or (go p1, go p2)
      end
      | _ -> Exists (x, go phi)
    end
    | Var _ | Arith _ | Bool _ | Pred _ -> phi
    | And (p1, p2) -> And (go p1, go p2)
    | Or (p1, p2) -> Or (go p1, go p2)
    | Abs (x, p) -> Abs (x, go p)
    | App (p1, p2) -> App (go p1, go p2)
  in
  go phi
  
let simplify_non_deterministic_branch hes =
  Hflz.merge_entry_rule hes
  |> List.map
      (fun {Hflz.var; body; fix} ->
        let body = simplify_non_deterministic_branch_sub body in
        {Hflz.var; body; fix}
      )
  |> Hflz.decompose_entry_rule

let eliminate_unused_bindings_sub phi =
  let rec go phi =
    let go_binding x p1 f =
      let (fvs, p1) = go p1 in
      if IdSet.exists fvs ~f:(fun x' -> Id.eq x' x) then
        (IdSet.remove fvs x, f x p1)
      else
        (fvs, p1) in
    match phi with
    | Hflz.Abs (x, p1) ->
      (* cannot simplify erase abs because it changes type *)
      let (fvs, p1) = go p1 in
      (IdSet.remove fvs x, Hflz.Abs (x, p1))
    | Forall (x, p1) ->
      go_binding x p1 (fun x p -> Hflz.Forall (x, p))
    | Exists (x, p1) ->
      go_binding x p1 (fun x p -> Hflz.Exists (x, p))
    | Var x -> (IdSet.singleton x, phi)
    | Bool _ -> (IdSet.empty, phi)
    | Or (p1, p2) ->
      let (s1, p1) = go p1 in
      let (s2, p2) = go p2 in
      (IdSet.union s1 s2, Or (p1, p2))
    | And (p1, p2) ->
      let (s1, p1) = go p1 in
      let (s2, p2) = go p2 in
      (IdSet.union s1 s2, And (p1, p2))
    | App (p1, p2) ->
      let (s1, p1) = go p1 in
      let (s2, p2) = go p2 in
      (IdSet.union s1 s2, App (p1, p2))
    | Arith a ->
      let s = IdSet.of_list @@ List.map Id.remove_ty @@ Arith.fvs a in
      (s, phi)
    | Pred (_, as') ->
      let s =
        IdSet.union_list @@ List.map (fun a ->
          IdSet.of_list @@ List.map Id.remove_ty @@ Arith.fvs a
        ) as' in
      (s, phi)
    in
  go phi |> snd

let eliminate_unused_bindings hes =
  hes
  |> log_hes "eliminate_unused_bindings (before):"
  |> Hflz.merge_entry_rule
  |> List.map
    (fun rule ->
      { rule with
        Hflz.body = eliminate_unused_bindings_sub rule.Hflz.body }
    )
  |> Hflz.decompose_entry_rule
  |> log_hes "eliminate_unused_bindings (after):"

let simplify (hes : Type.simple_ty Hflz.hes)=
  let hes = InlineExpansion.optimize hes in
  let hes = beta_hes hes in
  let hes = simple_partial_evaluate_hes hes in
  let hes = evaluate_trivial_boolean hes in
  (* let hes = Trans.Simplify.hflz_hes hes false in *)
  hes

let rec simplify_all hes =
  let hes' = simplify hes in
  if hes' <> hes then simplify_all hes' else hes'

let simplify_agg trivial_only ?(no_eliminate_unused_arguments=false) hes =
  let go hes =
    hes
    |> log_hes "simplify_agg (a0)"
    |> InlineExpansion.inline_non_recursive_variables false trivial_only
    |> log_hes "simplify_agg (a1)"
    |> beta_hes
    |> log_hes "simplify_agg (a2)"
    |> simple_partial_evaluate_hes
    |> log_hes "simplify_agg (a3)"
    |> evaluate_trivial_boolean
    |> log_hes "simplify_agg (a4)"
    |> eliminate_unused_bindings
    |> log_hes "simplify_agg (a5)"
    |> evaluate_trivial_fixpoints
    |> log_hes "simplify_agg (a6)"
  in
  hes
  |> log_hes "simplify_agg (1)"
  |> go |> go
  |> log_hes "simplify_agg (2)"
  |> inline_bottom |> go
  |> log_hes "simplify_agg (3)"
  |> simplify_non_deterministic_branch |> go
  |> log_hes "simplify_agg (4)"
  |> eliminate_unreachable_predicates
  |> log_hes "simplify_agg (5)"
  |> (if no_eliminate_unused_arguments then (fun x -> x) else Eliminate_unused_argument.eliminate_unused_argument ~id_type_map:IdMap.empty)
  |> log_hes "simplify_agg (6)"
  |> Constant_propagation.run |> go
  |> log_hes "simplify_agg (7)"
  |> Constant_propagation.run |> go
  |> log_hes "simplify_agg (8)"
  
