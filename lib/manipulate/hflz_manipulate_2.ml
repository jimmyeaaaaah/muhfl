module Print = Print_syntax
module Fixpoint = Hflmc2_syntax.Fixpoint
module Formula = Hflmc2_syntax.Formula
module IdSet = Hflmc2_syntax.IdSet

open Hflz_typecheck
open Hflz

(* boundが自由変数の値によって動的に変わる場合は対応しない *)
(* 現実的には、coe1 = 1 の定数のときのみ *)
let rec list_lower_to_upper lower upper =
  if lower > upper then []
  else lower :: (list_lower_to_upper (lower + 1) upper)
  
let list_product l1 l2 =
  List.map (fun e1 -> List.map (fun e2 -> (e1, e2)) l2) l1
  |> List.flatten

let eliminate_exists_by_assinging_hflz max_assign_value phi =
  let assigning_values = list_lower_to_upper (-max_assign_value) max_assign_value in
  let rec go (phi : Type.simple_ty Hflz.t): Type.simple_ty Hflz.t list =
    match phi with
    | Exists (x, p1) ->
      (* 存在する自由変数？ *)
      let results = go p1 in
      List.map
        (fun value ->
          List.map
            (* (substitute x value) *)
            (Hflmc2_syntax.Trans.Subst.Hflz.hflz (Hflmc2_syntax.IdMap.of_list [x, Arith (Int value)]))
            results
        )
        assigning_values
      |> List.flatten
    | Bool _ | Var _ | Arith _ | Pred _ -> [phi]
    | Or (p1, p2) ->
      list_product (go p1) (go p2)
      |> List.map (fun (a, b) -> Or (a, b))
    | And (p1, p2) ->
      list_product (go p1) (go p2)
      |> List.map (fun (a, b) -> And (a, b))
    | App (p1, p2) ->
      list_product (go p1) (go p2)
      |> List.map (fun (a, b) -> App (a, b))
    | Forall (x, p1) -> 
      go p1
      |> List.map (fun p1 -> Forall (x, p1))
    | Abs (x, p1) -> 
      go p1
      |> List.map (fun p1 -> Abs (x, p1))
  in
  go phi

let eliminate_exists_by_assinging max_assign_value (hes : Type.simple_ty Hflz.hes) =
  let entry, rules = hes in
  let rules = (mk_entry_rule entry)::rules in
  let ruless =
    List.map
      (fun rule ->
        let bodies = eliminate_exists_by_assinging_hflz max_assign_value rule.body in
        List.map (fun body -> { rule with body }) bodies
      )
      rules
  in
  List.fold_left
    (fun (acc: 'a list list) (rules: 'a list) ->
      match acc with
      | [] -> List.map (fun r -> [r]) rules
      | acc -> 
        list_product acc rules
        |> List.map (fun (a, rule) -> rule::a)
    )
    []
    ruless
  |> List.rev
  |> List.map decompose_entry_rule
