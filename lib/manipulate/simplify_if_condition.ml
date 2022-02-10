open Hflmc2_syntax
open Hflz

let log_src = Logs.Src.create "Optimizer"
module Log = (val Logs.src_log @@ log_src)

let log_string = Hflz_util.log_string Log.info

module FormulaSimplification = struct
  module C = Simplify_bound.ConvertLib

  let trans_pred_ used_variables op a1 a2 =
    match op with
    | Formula.Neq ->
      ("(not (" ^ C.trans_pred Formula.Eq ^ " " ^ C.trans_arith used_variables a1 ^ " " ^ C.trans_arith used_variables a2 ^ "))")
    | _ ->
      ("(" ^ C.trans_pred op ^ " " ^ C.trans_arith used_variables a1 ^ " " ^ C.trans_arith used_variables a2 ^ ")")

  let convert_to_smt2 (form : 'a t) =
    let used_variables = ref [] in
    let rec go form = match form with
      | Pred (op, [a1; a2]) ->
        trans_pred_ used_variables op a1 a2
      | Pred _ -> assert false
      | And (p1, p2) ->
        ("(and " ^ go p1 ^ " " ^ go p2 ^ ")")
      | Or (p1, p2) ->
        ("(or " ^ go p1 ^ " " ^ go p2 ^ ")")
      | Bool b ->
        (string_of_bool b)
      | _ -> assert false
    in
    let body = go form in
    let variables = Hflmc2_util.remove_duplicates (=) !used_variables in
    (
      (variables
      |> List.map C.string_of_id
      |> List.map (fun v -> "(declare-const " ^ v ^ " Int)\n")
      |> String.concat "") ^ "\n" ^
      "(assert " ^ body ^ ")\n(apply (then ctx-solver-simplify ctx-solver-simplify simplify))\n"
    ),
    variables

  let convert_from_smt2 variables sexp =
    let open Core in
    print_endline "sexp:";
    print_endline @@ Sexp.to_string sexp;
    let rec to_ors cs = match cs with
      | [] -> assert false
      | [x] -> x
      | x::xs -> Or (x, to_ors xs) in
    let rec to_ands cs = match cs with
      | [] -> assert false
      | [x] -> x
      | x::xs -> And (x, to_ands xs)
    in
    let get_var_id x =
      match List.filter ~f:(fun id -> String.(=) (C.string_of_id id) x) variables with
      | [x] -> x
      | [] -> failwith @@ "get_var_id: not found (" ^ x ^ ")"
      | _ -> failwith @@ "get_var_id: many found (" ^ x ^ ")" in
    let rec go_arith xs = match xs with
      | Sexp.List ((Atom op)::args) -> begin
        let op = C.to_op op in
        match op, List.map ~f:go_arith args with
        | Arith.Sub, [Arith.Int i] -> Int (-i)
        | Arith.Sub, [x] -> Arith.Op (Sub, [Int 0; x])
        | _, args -> begin
          assert (List.length args = 2);
          Arith.Op (op, args)
        end
      end
      | Atom i -> begin
        let expr =
          try
            Arith.Int (int_of_string i)
          with _ ->
            Var (get_var_id i)
          in
        expr
      end
      | _ -> failwith "go_arith"
    in
    let rec go sexp = match sexp with
      | Sexp.Atom "true" ->
        Bool true
      | Sexp.Atom "false" ->
        Bool false
      | Sexp.List (Atom "or"::args) ->
        let args = List.map ~f:go args in
        to_ors args
      | Sexp.List (Atom "and"::args) ->
        let args = List.map ~f:go args in
        to_ands args
      | Sexp.List ((Atom "not")::[List ((Atom pred)::x1::[x2])]) -> begin
        let pred = C.to_predicate pred |> Formula.negate_pred in
        let x1 = go_arith x1 in
        let x2 = go_arith x2 in
        Pred (pred, [x1; x2])
      end
      | Sexp.List ((Atom pred)::x1::[x2]) ->
        let pred = C.to_predicate pred in
        let x1 = go_arith x1 in
        let x2 = go_arith x2 in
        Pred (pred, [x1; x2])
      | _ -> assert false
    in
    go sexp

  let formula_fold func terms = match terms with
    | [] -> failwith "[formula_fold] Number of elements should not be zero."
    | term::terms ->
      List.fold_left func term terms
    
  let simplify_with_z3 z3_path formula =
    (* let rec go xs  *)
    let smt2, variables = convert_to_smt2 formula in
    print_endline "origin:";
    print_endline @@ Print_syntax.show_hflz formula;
    
    let file_name = Hflmc2_util.gen_temp_filename "/tmp/" ".tmp" in
    print_endline "simplify_bound_with_z3"; print_endline "input file_name"; print_endline file_name;
    Hflmc2_util.write_file file_name smt2;
    let output_path = Hflmc2_util.gen_temp_filename "/tmp/" ".tmp" in
    ignore @@ Unix.system @@ z3_path ^ " " ^ file_name ^ " pp.max_depth=10000 pp.min-alias-size=10000 > " ^ output_path;
    print_endline "output file_name"; print_endline output_path;
    let s = Hflmc2_util.read_file output_path in
    
    let sexp = C.parse_sexp s in
    let form = convert_from_smt2 variables sexp in
    form
end

let simplify_with_z3 z3_path cond =
  match cond with
  | Or _ | And _ -> FormulaSimplification.simplify_with_z3 z3_path cond
  | _ -> cond

type 'a condition =
  | CondOr  of 'a t
  | CondAnd of 'a t

let merge_conditions logical_connective_type conditions p =
  let rec go conditions body =
    match conditions with
    | x::xs -> begin
      match logical_connective_type, x with
      | `Or, CondOr t -> Or (t, go xs body)
      | `And, CondAnd t -> And (t, go xs body)
      | `Or, CondAnd _ -> go xs body
      | `And, CondOr _ -> go xs body
    end
    | [] -> body
  in
  go (List.rev conditions) p

let get_primitive_predicate phi =
  let exception NonPrimitive in
  let rec go phi = match phi with
    | Pred _ -> phi
    | Bool _ -> phi
    | Or (p1, p2) -> Or (go p1, go p2)
    | And (p1, p2) -> And (go p1, go p2)
    | _ -> raise NonPrimitive
  in
  try
    Some (go phi)
  with NonPrimitive ->
    None
  
let run_sub z3_path phi =
  let rec go conditions phi =
    match phi with
    | Or (p1, p2) -> begin
      let pred = get_primitive_predicate p1 in
      match pred with
      | Some pred -> begin
        print_endline @@ Print_syntax.show_hflz phi;
        let pred' =
          merge_conditions `Or conditions pred
          |> simplify_with_z3 z3_path in
        Or (pred', go (CondOr pred::conditions) p2)
      end
      | None ->
        Or (go conditions p1, go conditions p2)
    end
    | And (p1, p2) -> begin
      let pred = get_primitive_predicate p1 in
      match pred with
      | Some pred -> begin
        print_endline @@ Print_syntax.show_hflz phi;
        let pred' =
          merge_conditions `And conditions pred
          |> simplify_with_z3 z3_path in
        And (pred', go (CondAnd pred::conditions) p2)
      end
      | _ ->
        And (go conditions p1, go conditions p2)
    end
    | App (p1, p2) ->
      App (go [] p1, go [] p2)
    | Var _ | Arith _ -> phi
    | Bool _ -> phi
    | Pred _ -> phi
    | Abs (x, p) ->
      Abs (x, go [] p)
    | Exists (x, p) ->
      Exists (x, go [] p)
    | Forall (x, p) ->
      Forall (x, go [] p)
  in
  go [] phi

let run z3_path hes =
  let rules = merge_entry_rule hes in
  print_endline "[Before] Simplifed if conditions:";
  print_endline @@ Print_syntax.show_hes ~readable:true rules;
  let rules =
    List.map
      (fun {var; body; fix} ->
        let body = run_sub z3_path body in
        {var; body; fix}
      )
      rules in
  print_endline "[After] Simplifed if conditions:";
  print_endline @@ Print_syntax.show_hes ~readable:true rules;
  decompose_entry_rule rules
