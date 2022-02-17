open Hflmc2_syntax

let log_string s =
  ignore s; ()
  (* () ; print_endline s *)

type expansions =
  (unit Id.t, int, IdMap.Key.comparator_witness) Hflmc2_util.Map.t

type 'ty thflz =
  | Bool   of bool
  | Var    of 'ty Id.t
  | Or     of 'ty thflz * 'ty thflz
  | And    of 'ty thflz * 'ty thflz
  | Abs    of 'ty Type.arg Id.t * 'ty thflz * data option
  | Forall of 'ty Type.arg Id.t * 'ty thflz
  | Exists of 'ty Type.arg Id.t * 'ty thflz
  | App    of 'ty thflz * 'ty thflz
  | Arith  of Arith.t
  | Pred   of Formula.pred * Arith.t list
and data = (Type.simple_argty Id.t * value) list * Type.simple_ty Id.t
and value =
  | VThunk of Type.simple_ty thflz * (Type.simple_argty, value) Env.t ref
  | VFun of Type.simple_argty Id.t * Type.simple_ty thflz * (Type.simple_argty, value) Env.t ref * expansions * data option
  | VBool of bool
  | VInt of int
  | VUndefined

type 'ty thes_rule =
  { var  : 'ty Id.t
  ; body : 'ty thflz
  ; fix  : Fixpoint.t
  }
    
let max_expansion = ref 3
let max_expansion_show = ref 2
let shortcircuit = ref false

module Print_temp = struct
  open Print
  
  let rec hflz_ : (Prec.t -> 'ty Fmt.t) -> Prec.t -> 'ty thflz Fmt.t =
    fun format_ty_ prec ppf (phi : 'ty thflz) -> match phi with
      | Bool true -> Fmt.string ppf "true"
      | Bool false -> Fmt.string ppf "false"
      | Var x -> id ppf x
      | Or(phi1,phi2)  ->
          show_paren (prec > Prec.or_) ppf "@[<hv 0>%a@ || %a@]"
            (hflz_ format_ty_ Prec.or_) phi1
            (hflz_ format_ty_ Prec.or_) phi2
      | And (phi1,phi2)  ->
          show_paren (prec > Prec.and_) ppf "@[<hv 0>%a@ && %a@]"
            (hflz_ format_ty_ Prec.and_) phi1
            (hflz_ format_ty_ Prec.and_) phi2
      | Abs (x, psi, _) ->
          show_paren (prec > Prec.abs) ppf "@[<1>λ%a:%a.@,%a@]"
            id x
            (argty (format_ty_ Prec.(succ arrow))) x.ty
            (hflz_ format_ty_ Prec.abs) psi
      | Forall (x, psi) ->
          show_paren (prec > Prec.abs) ppf "@[<1>∀%a.@,%a@]"
            id x
            (hflz_ format_ty_ Prec.abs) psi
      | Exists (x, psi) ->
          show_paren (prec > Prec.abs) ppf "@[<1>∃%a.@,%a@]"
            id x
            (hflz_ format_ty_ Prec.abs) psi
      | App (psi1, psi2) ->
          show_paren (prec > Prec.app) ppf "@[<1>%a@ %a@]"
            (hflz_ format_ty_ Prec.app) psi1
            (hflz_ format_ty_ Prec.(succ app)) psi2
      | Arith a ->
          arith_ prec ppf a
      | Pred (pred, as') ->
          show_paren (prec > Prec.eq) ppf "%a"
            formula (Formula.Pred(pred, as'))

  let hflz : (Prec.t -> 'ty Fmt.t) -> 'ty thflz Fmt.t =
    fun format_ty_ -> hflz_ format_ty_ Prec.zero
end

let show_hflz = Hflmc2_util.fmt_string Print_temp.(hflz (fun _ _ _ -> ()))
let show_arith a = show_hflz (Arith a)

let rec show_value_ i expr =
  if i >= !max_expansion_show then "..."
  else match expr with
  | VThunk (body, env) ->
    show_env i !env ^ " \\" ^ show_hflz body
  | VFun (x, body, env, _, _) ->
    show_env i !env ^ " \\" ^ Id.to_string x ^ ". " ^ show_hflz body
  | VBool b -> string_of_bool b
  | VInt i -> string_of_int i
  | VUndefined -> "undefined"
and show_env i (env : (Type.simple_argty Id.t * value) list) =
  "{" ^ (List.map (fun (id, v) -> Id.to_string id ^ ": " ^ show_value_ (i + 1) v) env |> String.concat "; ") ^ "}"

let show_value expr = show_value_ 0 expr

let show_expansions_ expansions string_of_int =
  IdMap.fold expansions ~init:[] ~f:(fun ~key:k ~data:v acc -> (Id.to_string k ^ ": " ^ string_of_int v)::acc)
  |> String.concat "; "

let show_expansions expansions = show_expansions_ expansions string_of_int

let show_data data =
  let (expansions, data_acc) = data in
  let string_of_int_opt = function None -> "None" | Some i -> "Some(" ^ string_of_int i ^ ")" in
  (* ((Type.simple_ty Type.arg Id.t * value) list * int option) list *)
  "expansions: " ^ show_expansions_ expansions string_of_int_opt ^ ", " ^
  ((List.map
    (fun (env, var, ex_count) ->
      "(" ^
      "(" ^ Id.to_string var ^ ", " ^ show_env 0 env ^ "): " ^ string_of_int_opt ex_count
      ^ ")"
    )
    data_acc)
    |> String.concat "; "
  )

let rec evaluate_arith env expr =
  (* log_string "entry (arith):";
  log_string @@ "* expr: " ^ show_arith expr;
  log_string @@ "* env: " ^ show_env 0 env;
  log_string @@ ""; *)
  (match expr with
  | Arith.Int i -> i
  | Var x -> begin
    let v =
      try
        Env.lookup x env
      with Not_found -> failwith @@ "not found (" ^ Id.to_string x ^ ")" in
    match v with
    | VInt i -> i
    | _ -> failwith @@ "evaluate_arith: a variable " ^ Id.to_string x ^ " should be a integer value"
  end
  | Op (op, [expr1; expr2]) ->
    let v1 = evaluate_arith env expr1 in
    let v2 = evaluate_arith env expr2 in
    let op =
      match op with
      | Add -> (+)
      | Sub -> (-)
      | Mult -> ( * )
      | Div -> (/)
      | Mod -> (mod)
    in
    op v1 v2
  | Op _ -> assert false)

let is_counter_variable v =
  let name = v.Id.name in
  if
    (String.length name >= 3 && String.sub name 0 3 = "rec") ||
    (String.length name >= 5 && String.sub name 0 5 = "sssss")
    then true else false

let rec to_hflz expr =
  match expr with
  | Bool b -> Hflz.Bool b
  | Var v  -> Var v
  | Or (p1, p2) -> Or (to_hflz p1, to_hflz p2)
  | And (p1, p2) -> And (to_hflz p1, to_hflz p2)
  | Abs (x, p, _) -> Abs (x, to_hflz p)
  | Forall (x, p) -> Forall (x, to_hflz p)
  | Exists (x, p) -> Exists (x, to_hflz p)
  | App (p1, p2) -> App (to_hflz p1, to_hflz p2)
  | Arith a -> Arith a
  | Pred (op, xs) -> Pred (op, xs)


let extract_bound_predicates xs phi =
  let rec to_ors phi = match phi with
    | Hflz.Or (p1, p2) -> to_ors p1 @ to_ors p2
    | _ -> [phi]
  in
  let extract_pred p =
    print_endline @@ "B1:" ^ Print_syntax.show_hflz p;
    match p with
    | Hflz.Pred (op, [Var x'; a]) when (op = Le || op = Lt) && List.exists (Id.eq x') xs -> begin
      print_endline @@ "B2:" ^ Print_syntax.show_hflz (Arith a);
      match a with
      | Int _ -> Some (x', a)
      | Op (Add, [Op (Mult, [n; f]); Int _]) | Op (Add, [Int _; Op (Mult, [n; f])]) -> begin
        if IdSet.is_empty (Hflz.fvs (Arith n)) &&
            (not @@ IdSet.exists ~f:(fun x -> List.exists (Id.eq x) xs) (Hflz.fvs (Arith f))) then
          Some (x', a)
        else None
      end
      | _ -> None
    end
    | _ -> None
  in
  print_endline @@ "extract_bound_predicates: " ^ Hflmc2_util.show_list Id.to_string xs;
  let ors = to_ors phi in
  print_endline @@ "len: " ^ string_of_int (List.length ors);
  let preds = List.filter_map extract_pred ors in
  if List.length ors = List.length preds then Some (preds) else None

let get_bounds xs expr =
  let expr = to_hflz expr in
  log_string "get_bounds:";
  log_string @@ Print_syntax.show_hflz expr;
  match expr with
  | Hflz.Or (pred_expr, _) -> begin
    print_endline "AA1" ;
    log_string @@ Print_syntax.show_hflz pred_expr;
    match extract_bound_predicates xs pred_expr with
    | Some preds -> print_endline @@ "AA2 (" ^ Hflmc2_util.show_list Id.to_string xs ^ ")"; Some preds
    | None -> None
  end
  | _ -> None

let list_max ls =
  List.fold_left (fun m e -> if e > m then e else m) Int.min_int ls

let is_only_primitive_predicates e =
  let rec go p = match p with
    | Pred _ -> true
    | And (p1, p2) | Or (p1, p2) -> go p1 && go p2
    | _ -> false
  in
  go e

(* 各不動点の最大展開回数、全称量化子に代入した値 などを取得したい *)
let evaluate_hflz values_to_try' env expansions (expr : unit Type.ty thflz) ls =
  let rec evaluate_hflz_sub env expansions (expr : unit Type.ty thflz) ls =
    let get_bool_value = function
      | VBool b -> Some b
      | VUndefined -> None
      | _ -> assert false
    in
    let to_opt_expansions expansions =
      IdMap.map expansions ~f:(fun v -> Some v)
    in
    let merge_data data1 data2 =
      let (expansions1, data_acc1) = data1 in
      let (expansions2, data_acc2) = data2 in
      let expansions =
        IdMap.mapi
          expansions1
          ~f:(fun ~key:k ~data:v1 ->
            let v2 = IdMap.find_exn expansions2 k in
            if v1 < v2 then v2 else v1
          ) in
      expansions, data_acc1 @ data_acc2
    in
    let convert_forall_exists x e values_to_try =
      let datas, vs =
        List.map
          (fun value ->
            evaluate_hflz_sub (Env.update [x, VInt value] env) expansions e ls
          )
          values_to_try
        |> List.split in
      let data =
        match datas with
        | data::datas ->
          List.fold_left
            (fun acc data -> merge_data acc data)
            data
            datas
        | [] -> assert false in
      data, vs
      (* let v =
        in
      log_string "vvvv:";
      log_string @@ show_value v;
      data, v *)
    in
    (* let data_empty =
      let empty_e = IdMap.of_list (List.map (fun var -> Id.remove_ty var, Some 0) (IdMap.keys expansions)) in
      empty_e, [] 
    in *)
    log_string "entry:";
    log_string @@ "* expr: " ^ show_hflz expr;
    log_string @@ "* env: " ^ show_env 0 env;
    log_string @@ "* expansions: " ^ show_expansions expansions;
    log_string @@ "* ls: " ^ Hflmc2_util.show_list (fun s -> s) ls;
    log_string @@ "";
    let data_, v_ =
      match expr with
      | App (expr1, expr2) -> begin
        let data1, v1 = evaluate_hflz_sub env expansions expr1 ("App 1 (first)"::ls) in
        log_string @@ "result: " ^ show_value v1;
        (* TODO: dataをマージ *)
        match v1 with
        | VFun (x, e, fun_env, expansions', origin_opt) -> begin
          let data, v3 =
            match expr2 with
            | Arith a ->
              let a = evaluate_arith env a in
              let data3, v3 = evaluate_hflz_sub (Env.update [(x, VInt a)] !fun_env) expansions' e ("App 1 (int)"::ls) in
              merge_data data1 data3, v3
            | _ ->
              let data2, v = evaluate_hflz_sub env expansions expr2 ("App 2"::ls) in
              let data3, v3 = evaluate_hflz_sub (Env.update [(x, v)] !fun_env) expansions' e ("App 1 (fun)"::ls) in
              merge_data (merge_data data1 data2) data3, v3 in
          match origin_opt with
          | Some (integer_env, x) ->
            let (expansions', data_acc) = data in
            let ex_c_opt = IdMap.find_exn expansions' (Id.remove_ty x) in
            let data_acc = (integer_env, x, ex_c_opt) :: data_acc in
            let expansions' =
              IdMap.update
                expansions'
                (Id.remove_ty x)
                ~f:(fun _ -> None) in
            (expansions', data_acc), v3
          | None -> data, v3
        end
        | VUndefined -> begin
          let data =
            match expr2 with
            | Arith _ -> data1
            | _ ->
              let data2, _ = evaluate_hflz_sub env expansions expr2 ("App 2"::ls) in
              (merge_data data1 data2) in
          data, VUndefined
        end
        | _ -> assert false
      end
      | Arith _ -> assert false
      | Pred (p, [a1; a2]) -> begin
        let v1 = evaluate_arith env a1 in
        let v2 = evaluate_arith env a2 in
        let b =
          match p with
          | Eq -> (=)
          | Neq -> (<>)
          | Le -> (<=)
          | Ge -> (>=)
          | Lt -> (<)
          | Gt -> (>) in
        (to_opt_expansions expansions, []), VBool (b v1 v2)
      end
      | Bool b -> (to_opt_expansions expansions, []), VBool b
      | Var x -> begin
        let v =
          try
            Env.lookup x env
          with Not_found ->
            failwith @@ "Not Found: " ^ Id.to_string x ^ ", " ^ Hflmc2_util.show_list (fun s -> s) ls
          in
        log_string @@ "Var: " ^ Id.to_string x ^ ": " ^ show_value v;
        match IdMap.find expansions x with
        | Some c -> begin
          (* variable in fixpoint equations *)
          if c >= !max_expansion then
            (
              IdMap.update
                (to_opt_expansions expansions)
                (Id.remove_ty x)
                ~f:(fun _ -> None),
              []
            ), VUndefined
          else begin
            match v with
            | VThunk (body, thunk_env) -> begin
              let expansions =
                IdMap.update
                  expansions
                  (Id.remove_ty x)
                  ~f:(function Some v -> v + 1 | None -> assert false) in
              print_endline @@ "expand (" ^ Id.to_string x ^ ")";
              (* 各不動点の最大展開回数（最初の出現ごとに別）、それより深い場所の展開についてのデータ *)
              let data, v = evaluate_hflz_sub !thunk_env expansions body ("Var Thunk 1"::ls) in
              (* Absのときは、 data = (expansions, []) *)
              if c = 0 then (
                (* first expansion *)
                let integer_env =
                  List.filter 
                    (fun (id, _) ->
                      match id.Id.ty with
                      | Type.TySigma _ -> false
                      | TyInt -> true
                    )
                    (Env.entries env)
                  |> Env.create in
                match v with
                | VFun (x1, e, x3, x4, None) -> begin
                  let rec go p = match p with
                    | Abs (x', p, None) -> begin
                      match p with
                      | Abs _ -> Abs (x', go p, None) (* TODO: data集約実装が雑なので改善 *)
                      | _ -> Abs (x', p, Some (integer_env, x))
                    end
                    | Abs (_, _, _) -> assert false
                    | _ -> p
                  in
                  data, VFun (x1, go e, x3, x4, Some (integer_env, x))
                end
                | VFun _ -> assert false
                | _ ->
                  let (expansions', data_acc) = data in
                  (* 対象の不動点のみ（スコープの問題） *)
                  let ex_c_opt = IdMap.find_exn expansions' (Id.remove_ty x) in
                  let data_acc = (integer_env, x, ex_c_opt) :: data_acc in
                  let expansions' =
                    IdMap.update
                      expansions'
                      (Id.remove_ty x)
                      ~f:(fun _ -> None) in
                  (expansions', data_acc), v
              ) else
                (data, v)
            end
            | _ -> assert false
          end
        end
        | None -> (to_opt_expansions expansions, []), v
      end
      | Or (e1, e2) -> begin
        (* if is_only_primitive_predicates e2 then swap e1 e2 *)
        let data1, v1 = evaluate_hflz_sub env expansions e1 ("OR 1"::ls) in
        let v1_opt = get_bool_value v1 in
        match v1_opt with
        | Some true when !shortcircuit -> log_string @@ "ORV 3[" ^ show_hflz e1 ^ ", " ^ show_hflz e2; log_string @@ show_data data1; data1, VBool true
        | _ -> begin
          let data2, v2 = evaluate_hflz_sub env expansions e2 ("OR 2"::ls) in
          let v2_opt = get_bool_value v2 in
          let (a, ret_v) =
            match v1_opt, v2_opt with
            | None, None -> log_string @@ "ORV 1[" ^ show_hflz e1 ^ ", " ^ show_hflz e2; merge_data data1 data2, VUndefined
            | None, Some true -> log_string @@ "ORV 2[" ^ show_hflz e1 ^ ", " ^ show_hflz e2; log_string @@ show_data data1; log_string @@ show_data data2; data2, VBool true
            | Some true, None -> log_string @@ "ORV 3-2[" ^ show_hflz e1 ^ ", " ^ show_hflz e2; log_string @@ show_data data1; data1, VBool true
            | None, Some false -> log_string @@ "ORV 4[" ^ show_hflz e1 ^ ", " ^ show_hflz e2; merge_data data1 data2, VUndefined
            | Some false, None -> log_string @@ "ORV 5[" ^ show_hflz e1 ^ ", " ^ show_hflz e2; merge_data data1 data2, VUndefined
            | Some b1, Some b2 -> log_string @@ "ORV 6 (" ^ string_of_bool b1 ^ "," ^ string_of_bool b2 ^ ")[" ^ show_hflz e1 ^ ", " ^ show_hflz e2; merge_data data1 data2, VBool (b1 || b2)
            in
          log_string @@ show_data a;
          a, ret_v
        end
      end
      | And (e1, e2) -> begin
        let data1, v1 = evaluate_hflz_sub env expansions e1 ("AND 1"::ls) in
        let v1_opt = get_bool_value v1 in
        match v1_opt with
        | Some false when !shortcircuit ->
          log_string @@ "ANDV 3[" ^ show_hflz e1 ^ ", " ^ show_hflz e2; data1, VBool false
        | _ -> begin
          let data2, v2 = evaluate_hflz_sub env expansions e2 ("AND 2"::ls) in
          let v2_opt = get_bool_value v2 in
          let ret_v =
            match v1_opt, v2_opt with
            | None, None -> log_string @@ "ANDV 1[" ^ show_hflz e1 ^ ", " ^ show_hflz e2; merge_data data1 data2, VUndefined
            | None, Some false -> log_string @@ "ANDV 2[" ^ show_hflz e1 ^ ", " ^ show_hflz e2; data2, VBool false
            | Some false, None -> log_string @@ "ANDV 3-2[" ^ show_hflz e1 ^ ", " ^ show_hflz e2; data1, VBool false
            | None, Some true -> log_string @@ "ANDV 4[" ^ show_hflz e1 ^ ", " ^ show_hflz e2; merge_data data1 data2, VUndefined
            | Some true, None -> log_string @@ "ANDV 5[" ^ show_hflz e1 ^ ", " ^ show_hflz e2; merge_data data1 data2, VUndefined
            | Some b1, Some b2 -> log_string @@ "ANDV 6 (" ^ string_of_bool b1 ^ "," ^ string_of_bool b2 ^ ")[" ^ show_hflz e1 ^ ", " ^ show_hflz e2; merge_data data1 data2, VBool (b1 && b2)
            in
          ret_v
        end
      end
      | Abs (x, e, d) ->
        log_string "ABS:";
        log_string @@ "1: " ^ show_hflz (Abs (x, e, d));
        log_string @@ "2: " ^ show_hflz e;
        log_string @@ "3: " ^ show_value @@ VFun (x, e, ref env, expansions, d);
        (to_opt_expansions expansions, []), VFun (x, e, ref env, expansions, d)
      | Forall (x, e) -> begin
        let aggregate_values vs = if List.exists ((=)(VBool false)) vs then VBool false else VUndefined in
        (* let values_to_try = [1; 2; 4; 8] in *)
        (* boundであれば、単調なので、値を一つだけ調べればよい *)
        log_string @@ "A1 (" ^ Id.to_string x ^ "):";
        if is_counter_variable x then begin
          log_string "A2:";
          (* get subsequent counter variables *)
          let rec get_counters acc p = match p with
            | Forall (x, p) when is_counter_variable x -> (get_counters (x::acc) p)
            | _ -> acc, p
          in
          let counters, body = get_counters [] expr in
          match get_bounds counters body with
          | Some boundss -> begin
            log_string "A3:";
            let max_bounds =
              List.map
                (fun x ->
                  let bounds = List.find_all (fun (k, _v) -> Id.eq k x) boundss in
                  let bound_values = List.map (fun (_, bound) -> evaluate_arith env bound) bounds in
                  (x, VInt (list_max bound_values))
                )
                counters in
            log_string @@ Hflmc2_util.show_list (fun (k, v) -> Id.to_string k ^ ": " ^ show_value v) max_bounds;
            let data, vs =
              evaluate_hflz_sub (Env.update max_bounds env) expansions body ls in
            data, vs
          end
          | None ->
            (* fall back *)
            log_string "A4:";
            convert_forall_exists x e values_to_try' |> (fun (data, vs) -> data, aggregate_values vs)
        end else begin
          log_string "A5:";
          convert_forall_exists x e values_to_try' |> (fun (data, vs) -> data, aggregate_values vs)
        end
      end
      | Exists (x, e) -> begin (* TODO: 直す? *)
        let aggregate_values vs = if List.exists ((=)(VBool true)) vs then VBool true else VUndefined in
        log_string "C1:";
        convert_forall_exists x e values_to_try' |> (fun (data, vs) -> data, aggregate_values vs)
      end
      | Pred _ -> assert false
    in
    log_string "out:";
    log_string @@ "* expr: " ^ show_hflz expr;
    log_string @@ "* env: " ^ show_env 0 env;
    log_string @@ "* expansions: " ^ show_expansions expansions;
    log_string @@ "* ls: " ^ Hflmc2_util.show_list (fun s -> s) ls;
    log_string @@ "* value: " ^ show_value v_;
    log_string @@ "";
    data_, v_
  in
  evaluate_hflz_sub env expansions expr ls 

let evaluate_hes_ (hes : Type.simple_ty thflz * Type.simple_ty thes_rule list) try_values =
  let (entry, rules) = hes in
  let global_list =
    List.map
      (fun {var; body; _} ->
        {var with ty=Type.TySigma var.ty},
        VThunk (body, ref [])
      )
      rules
    in
  List.iter
    (function
      | (_, VThunk (_, r)) ->
        r := Env.create global_list
      | _ -> assert false
    )
    global_list;
  let expansions =
    IdMap.of_list (List.map (fun {var; _} -> Id.remove_ty var, 0) rules) in
  let d, v = evaluate_hflz try_values (Env.create global_list) expansions entry [] in
  print_endline "show_value";
  print_endline @@ show_value v;
  print_endline "show_data";
  print_endline @@ show_data d

let translate_hes (hes : Type.simple_ty Hflz.t * Type.simple_ty Hflz.hes_rule list) =
  let rec go_phi phi = match phi with
    | Hflz.Bool b -> Bool b
    | Var v -> Var v
    | Or (p1, p2) -> Or (go_phi p1, go_phi p2)
    | And (p1, p2) -> And (go_phi p1, go_phi p2)
    | Abs (x, p) -> Abs (x, go_phi p, None)
    | Forall (x, p) -> Forall (x, go_phi p)
    | Exists (x, p) -> Exists (x, go_phi p)
    | App (p1, p2) -> App (go_phi p1, go_phi p2)
    | Arith a -> Arith a
    | Pred (op, xs) -> Pred (op, xs)
  in
  let (entry, rules) = hes in
  let rules = List.map (fun {Hflz.var; body; fix} -> {var; body = go_phi body; fix}) rules in
  (go_phi entry, rules)
  
let evaluate_hes (hes : Type.simple_ty Hflz.t * Type.simple_ty Hflz.hes_rule list) try_values =
  let hes = translate_hes hes in
  evaluate_hes_ hes try_values
