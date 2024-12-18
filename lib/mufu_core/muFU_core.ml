open Hflmc2_syntax
open Transformer
   
module H = Raw_hflz
module P = Printer

let make_head rule =
  let args = rule.H.args in
  let var = rule.H.var in
  let body = List.fold_left (fun body arg -> H.mk_forall arg body) rule.H.body args in
  let newrule = {H.var=var; args=[]; fix=Fixpoint.Greatest; body=body} in
  newrule
;;

let transform (hes : H.hes) env : H.hes_rule list * string =
  let aux, goals = Seperator.seperate_goal_and_defs hes in
  let goal = List.hd goals in
  
  let alldefs : H.hes_rule list = transform_hes aux goal env in
  
  let head = List.hd alldefs |> make_head in
  let result = head::List.tl alldefs in
  
  let outtxt1 = P.pp_list ~sep:"\n" P.pp_rule result in

  let outtxt = "%HES\n" ^ outtxt1 in 
  (* outtxt |> P.dbgn "Result"; *)

  result, outtxt
  ;;
