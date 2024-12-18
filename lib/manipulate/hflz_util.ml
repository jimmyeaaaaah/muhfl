open Hflmc2_syntax
open Hflz

type variable_type =
  (* | VTOrdinal
  | VTRec *)
  | VTVarMax of Arith.t list
  | VTHigherInfo of (unit Id.t * unit Id.t list) option

let show_variable_type t = 
  match t with
  | VTHigherInfo x_xs ->
    "VTHigherInfo " ^
    (match x_xs with None -> "_" | Some (x, xs) -> "(" ^ x.Id.name ^ ", " ^ Hflmc2_util.show_list ~sep:";" (fun x -> x.Id.name) xs ^ ")")
  | VTVarMax a -> "VTVarMax (" ^ Hflmc2_util.show_list ~sep:"; " (fun a -> Print_syntax.show_hflz (Arith a)) a ^ ")"

let lift_id id =
  { id with Id.ty = Type.TySigma id.Id.ty}

let unlift_id id =
  { id with Id.ty = Type.unsafe_unlift id.Id.ty}

let get_dependency_graph (hes : 'a hes_rule list) =
  let preds = List.mapi (fun i {var; _} -> (i, var)) hes in
  let graph_size = List.length hes in
  let graph = Mygraph.init graph_size in
  (* 参照の依存グラフを作成 *)
  List.iteri
    (fun index {body; _} -> 
      fvs body
      |> IdSet.to_list
      |> List.filter_map
        (fun v ->
          List.find_opt (fun (_, v') -> Id.eq v' v) preds
        )
      |> List.iter (fun (i', _) -> 
        Mygraph.add_edge index i' graph
      )
    )
    hes;
  preds, graph

let show_hflz hflz = show Type.pp_simple_ty hflz

let get_hflz_type phi =
  let open Type in
  let rec go phi = match phi with
    | Bool   _ -> TyBool ()
    | Var    v -> v.ty
    | Or (f1, f2)  -> begin
      assert ((go f1) = TyBool ());
      assert ((go f2) = TyBool ());
      TyBool ()
    end
    | And (f1, f2) -> begin
      assert ((go f1) = TyBool ());
      assert ((go f2) = TyBool ());
      TyBool ()
    end
    | Abs (x, f1)  -> TyArrow (x, go f1)
    | Forall (_, f1) -> begin
      let ty = go f1 in
      if ty <> TyBool () then print_endline @@ Type.show_simple_ty ty;
      assert ((go f1) = TyBool ());
      TyBool ()
    end
    | Exists (_, f1) -> begin
      assert ((go f1) = TyBool ());
      TyBool ()
    end
    | App (f1, f2)   -> begin
      let ty1 = go f1 in
      match ty1 with
      | TyArrow (x, ty1') -> begin
        (match x.ty with
        | TyInt -> (match f2 with Arith _ -> () | _ -> failwith @@ "Illegal type (App, Arrow) (ty1=TyInt, ty2=(not integet expression)) (expression: " ^ show_hflz phi ^ ")")
        | TySigma t -> (
          let sty2 = go f2 in
          if not @@ eq_modulo_arg_ids t sty2 then (
            failwith @@ "Type assertion failed (ty1=" ^ show_simple_ty t ^ ", ty2=" ^ show_simple_ty sty2 ^ ")"
          )
        )
        );
        ty1'
      end
      | _ -> failwith "Illegal type (App)"
      
    end
    | Pred _ -> TyBool ()
    | Arith _ -> failwith "Illegal type (Arith)"
  in
  go phi


open Hflmc2_syntax

let assign_unique_variable_id_sub id_change_map env phi =
  let to_ty ty = match ty with
    | Type.TyInt -> failwith "ty"
    | TySigma s -> s
  in
  let to_arithty ty = match ty with
    | Type.TyInt -> `Int
    | TySigma _ -> failwith "arithty"
  in
  let rec go env body = match body with
    | Hflz.Bool b -> Hflz.Bool b
    | Var v -> begin
      match List.find_all (fun (e, _) -> Id.eq e v) env with
      | [(_, v)] -> Var ({v with ty = to_ty v.Id.ty})
      | [] -> failwith @@ "unbound: " ^ Id.to_string v
      | _ -> assert false
    end
    | Or (p1, p2) -> Or (go env p1, go env p2)
    | And (p1, p2) -> And (go env p1, go env p2)
    | Abs (x, p) ->
      let x' = { x with id = Id.gen_id () } in
      id_change_map := (Id.remove_ty x, x') :: !id_change_map;
      Abs (x', go ((Id.remove_ty x, x') :: env) p)
    | Forall (x, p) ->
      let x' = { x with id = Id.gen_id () } in
      id_change_map := (Id.remove_ty x, x') :: !id_change_map;
      Forall (x', go ((Id.remove_ty x, x') :: env) p)
    | Exists (x, p) ->
      let x' = { x with id = Id.gen_id () } in
      id_change_map := (Id.remove_ty x, x') :: !id_change_map;
      Exists (x', go ((Id.remove_ty x, x') :: env) p)
    | App (p1, p2) -> App (go env p1, go env p2)
    | Arith a -> Arith (go_arith env a)
    | Pred (e, ps) -> Pred (e, List.map (go_arith env) ps)
  and go_arith env a = match a with
    | Int i -> Int i
    | Var v -> begin
      match List.find_all (fun (e, _) -> Id.eq e v) env with
      | [(_, v)] -> Var ({v with ty = to_arithty v.Id.ty})
      | [] -> failwith @@ "unbound: " ^ Id.to_string v
      | _ -> assert false
    end
    | Op (o, ps) -> Op (o, List.map (go_arith env) ps)
  in
  go env phi

let assign_unique_variable_id (hes : Type.simple_ty Hflz.hes_rule list): Type.simple_ty Hflz.hes_rule list * (unit Id.t * Type.simple_ty Type.arg Id.t) list =
  let to_ty ty = match ty with
    | Type.TyInt -> failwith "ty"
    | TySigma s -> s
  in
  let id_change_map = ref [] in
  let global_env =
    List.map (fun {Hflz.var; _} ->
      (Id.remove_ty var, {Id.name = var.name; id = Id.gen_id (); ty = Type.TySigma (var.Id.ty)})
    ) hes in
  
  let hes =
    List.map (fun {Hflz.var; body; fix} ->
      let body = assign_unique_variable_id_sub id_change_map global_env body in
      let var =
        match List.find_all (fun (e, _) -> Id.eq e var) global_env with
        | [(_, v)] -> {v with ty = to_ty v.Id.ty}
        | [] -> failwith @@ "unbound: " ^ Id.to_string var
        | _ -> assert false
      in
      {Hflz.var; body; fix}
    ) hes in
  let id_change_map = global_env @ List.rev !id_change_map in
  hes, id_change_map


let with_rules f hes = hes |> merge_entry_rule |> f |> decompose_entry_rule

let beta_id_type_map id_type_map x phi2 =
  IdMap.map 
    id_type_map
    ~f:(function
      | VTVarMax vs -> begin
        let xs =
          List.map  
            (fun v ->
              match Trans.Subst.Hflz.hflz (IdMap.of_list [x, phi2]) (Arith v) with
              | Arith v' -> begin
                (* if v <> v' then print_endline @@ "subst from " ^ Id.to_string x ^ " to " ^ Print_syntax.show_hflz phi2; *)
                v'
              end
              | _ -> assert false
            )
            vs in
        VTVarMax xs
      end
      | VTHigherInfo x_xs ->
        let x_xs = Option.bind x_xs (fun (x', xs') ->
          if Id.eq x' x then ((* print_endline "AA 2";*) None) else begin
            let xs' =
              List.filter_map
                (fun x'' ->
                  if Id.eq x'' x then
                    ((* print_endline "AA 1";*) None)
                  else Some x''
                )
                xs' in
            Some (x', xs')
          end
        ) in
        VTHigherInfo x_xs
    )

let rec beta id_type_map (phi : 'a Hflz.t) : ('b * 'a Hflz.t ) =
  match phi with
  | Or (phi1, phi2) ->
    let id_type_map, phi1 = beta id_type_map phi1 in
    let id_type_map, phi2 = beta id_type_map phi2 in
    id_type_map, Or (phi1, phi2)
  | And(phi1, phi2) ->
    let id_type_map, phi1 = beta id_type_map phi1 in
    let id_type_map, phi2 = beta id_type_map phi2 in
    id_type_map, And (phi1, phi2)
  | App(phi1, phi2) -> begin
    let id_type_map, phi1 = beta id_type_map phi1 in
    let id_type_map, phi2 = beta id_type_map phi2 in
    let reduced = ref false in
    let rec go acc phi1 = match phi1 with
      | Forall (x, phi1) ->
        let id_type_map, phi1 = go (x::acc) phi1 in
        id_type_map, Forall (x, phi1)
      | Exists (x, phi1) ->
        let id_type_map, phi1 = go (x::acc) phi1 in
        id_type_map, Exists (x, phi1)
      | Abs(x, phi1) -> begin
        let fvs = fvs_with_type phi2 in
        (* print_endline @@ Print_syntax.show_hflz phi1;
        print_endline "fvs:"; Hflmc2_util.print_list Id.to_string fvs;
        print_endline "acc:"; Hflmc2_util.print_list Id.to_string acc; *)
        if List.exists (fun a -> List.exists (fun v -> Id.eq a v) acc) fvs then failwith "[beta] free variable collision";
        reduced := true;
        let id_type_map = beta_id_type_map id_type_map x phi2 in
        (* print_endline "SUBST";
        print_endline @@ Print_syntax.show_hflz phi1;
        print_endline @@ Id.to_string x;
        print_endline @@ Print_syntax.show_hflz phi2; *)
        beta id_type_map @@ Trans.Subst.Hflz.hflz (IdMap.of_list [x, phi2]) phi1
      end
      | phi1 -> id_type_map, phi1 in
    let id_type_map, res = go [] phi1 in
    if !reduced then
      beta id_type_map res
    else (
      (* Log.info begin fun m -> m ~header:"not done" "%a" Print.(hflz simple_ty_) (App (phi1, phi2)) end; *)
      id_type_map, App (phi1, phi2))
  end
  | Abs(x, phi) ->  
    let id_type_map, phi = beta id_type_map phi in
    id_type_map, Abs(x, phi)
  | Forall (x, phi) ->
    let id_type_map, phi = beta id_type_map phi in
    id_type_map, Forall (x, phi)
  | Exists (x, phi) ->
    let id_type_map, phi = beta id_type_map phi in
    id_type_map, Exists (x, phi)
  | Bool _ | Var _ | Arith _ | Pred _ -> id_type_map, phi

let update_id_type_map (id_type_map : (unit Id.t, variable_type, IdMap.Key.comparator_witness) Base.Map.t) (id_change_map : (unit Id.t * Type.simple_ty Type.arg Id.t) list) =
  (* id -> [id] というmap *)
  (* id_change_map のキーは重複がある可能性がある（元々IDが重複していた変数があった場合）。最も左のキー（関数環境, 外側の変数 -> 内側の変数の順になっている）を使う *)
  let update_id id =
    match List.assoc_opt (Id.remove_ty id) id_change_map with
    | Some key -> Some key
    | None -> None (* instantiate-exists で変数が消去された場合 *)
  in
  let m =
    IdMap.fold
      id_type_map
      ~init:IdMap.empty
      ~f:(fun ~key ~data m ->
        let key =
          match List.assoc_opt key id_change_map with
          | Some key -> Some key
          | None -> print_endline @@ "update_id_type_map: NOT FOUND (" ^ Id.to_string key ^ ")"; None in
        match key with
        | Some key -> begin
          let data =
            match data with
            | VTVarMax ariths -> begin
              VTVarMax
                (List.filter_map
                  (fun arith ->
                    match arith with
                    | Arith.Var id ->
                      update_id id |> Option.map (fun x -> Arith.Var {x with Id.ty=`Int})
                    | _ ->
                      let arith =
                        List.fold_left
                          (fun acc (x1, x2) ->
                            Trans.Subst.Hflz.arith (IdMap.of_list [x1, (Arith (Var {x2 with Id.ty=`Int}))]) acc
                          )
                          arith
                          id_change_map in
                      (* assert (IdSet.is_empty (Hflz.fvs (Arith arith))); *)
                      Some arith
                  )
                  ariths)
            end
            | VTHigherInfo x_xs -> begin
              let x_xs = Option.map (fun (x, xs) ->
                match update_id x with
                | Some x -> begin
                  let xs =
                    List.filter_map
                      (fun x -> update_id x |> Option.map Id.remove_ty)
                      xs in
                  (Id.remove_ty x, xs)
                end
                | None -> assert false (* ? *)
              ) x_xs in
              VTHigherInfo x_xs
            end
          in
          if IdMap.mem m (Id.remove_ty key) then (
            print_endline @@ "already exists: " ^ Id.to_string key;
            m
          ) else
            IdMap.add m key data
        end
        | None -> m
      )
  in
  m


(* let log_string
    (log_fun : ((?header:string -> ?tags:Logs.Tag.set -> ('a, Format.formatter, unit, unit) format4 -> 'a) -> unit) -> unit)
    ?header
    s =
  log_fun (fun m -> m ?header "%s" s) *)

let log_string
    (log_fun : ((?header:string -> ?tags:Logs.Tag.set -> ('a, Format.formatter, unit, unit) format4 -> 'a) -> unit) -> unit)
    ?header
    s =
  let reporter = Logs.reporter () in
  let r = Hflmc2_util.get_reporter "@[<v 2>[%a] @]" in
  Logs.set_reporter r;
  log_fun (fun m -> m ?header "%s" s);
  Logs.set_reporter reporter

let get_hflz_size_sub phi =
  let sum = List.fold_left (fun acc e -> acc + e) 0 in
  let rec go phi = match phi with
    | Var _ | Bool _ -> 1
    | Or (p1, p2) -> go p1 + go p2 + 1
    | And (p1, p2) -> go p1 + go p2 + 1
    | Abs (_, p) -> go p + 1
    | Forall (_, p) -> go p + 1
    | Exists (_, p) -> go p + 1
    | App (p1, p2) -> go p1 + go p2 + 1
    | Arith a -> go_arith a
    | Pred (_, as') -> (List.map go_arith as' |> sum) + 1
  and go_arith a = match a with
    | Var _ | Int _ -> 1
    | Op (_, as') -> (List.map go_arith as' |> sum) + 1
  in
  go phi

let get_hflz_size hes =
  let sum = List.fold_left (fun acc e -> acc + e) 0 in
  hes
  |> Hflz.merge_entry_rule
  |> List.map (fun {body;_} -> get_hflz_size_sub body)
  |> sum

let extract_bound_predicates x phi =
  let rec to_ors phi = match phi with
    | Hflz.Or (p1, p2) -> to_ors p1 @ to_ors p2
    | _ -> [phi]
  in
  let extract_pred p =
    match p with
    | Hflz.Pred (op, [Var x'; a]) when (op = Le || op = Lt) && Id.eq x' x -> begin
      match a with
      | Int _ -> Some a
      | Op (Add, [Op (Mult, [n; f]); Int _]) | Op (Add, [Int _; Op (Mult, [n; f])]) -> begin
        if IdSet.is_empty (Hflz.fvs (Arith n)) &&
            (not @@ IdSet.exists ~f:(Id.eq x) (Hflz.fvs (Arith f))) then
          Some a
        else None
      end
      | _ -> None
    end
    | _ -> None
  in
  let ors = to_ors phi in
  let preds = List.filter_map extract_pred ors in
  if List.length ors = List.length preds then Some preds else None

let decompose_ors x phi =
  match phi with
  | Hflz.Or (predicates, body) -> begin
    let preds = extract_bound_predicates x predicates in
    match preds with
    | Some preds ->
      let _, body = beta IdMap.empty body in
      if (not @@ IdSet.exists ~f:(Id.eq x) (Hflz.fvs body)) then Some (preds, body)
      else None
    | None -> None
  end
  | _ -> None