open Hflmc2_syntax
open Hflz

let log_src = Logs.Src.create "Optimizer"
module Log = (val Logs.src_log @@ log_src)

let log_string = Hflz_util.log_string Log.info

let get_occurrences hes =
  let rules = merge_entry_rule hes in
  let map = Hashtbl.create 10 in
  let rec go phi = match phi with
    | App (phi1, phi2) -> begin
      let rec go_ acc phi = match phi with
        | App (p1, p2) ->
          go_ (p2::acc) p1
        | _ -> acc, phi
      in
      let args, p = go_ [] phi in
      match p with
      | Var x when Id.is_pred_name x.name -> (
        Hashtbl.add map x args;
        List.iter go args
      )
      | _ ->
        (go phi1; go phi2)
    end
    | Var x when Id.is_pred_name x.name ->
      Hashtbl.add map x []
    | Var _ | Arith _ | Pred _ | Bool _ -> ()
    | And (p1, p2) | Or (p1, p2) ->
        (go p1; go p2)
    | Abs (_, p) -> go p
    | Forall (_, p) -> go p
    | Exists (_, p) -> go p
  in
  List.iter (fun {body; _} -> go body) rules;
  let results =
    List.map
      (fun {var; _} ->
        let l =
          Hashtbl.find_all map var
          |> Hflmc2_util.remove_duplicates (=) in
        (var, l)
      )
      rules in
  let () =
    log_string "Occurrences";
    List.iter
      (fun (k, v) ->
        log_string @@ Id.to_string k ^ ":";
        List.iter
          (fun args ->
            log_string @@ Hflmc2_util.show_list (fun t -> Print_syntax.show_hflz t) args
          )
          v
      )
      results in
  results

module Mygraph2 = struct
  let init () =
    Hashtbl.create 10
  
  let add_edge edges from' to' =
    Hashtbl.add edges from' to'

  let reachable_nodes_from edges start =
    (* BFS *)
    let module Queue = Core.Queue in
    let queue = Queue.create () in
    let used = Hashtbl.create 10 in
    Queue.enqueue queue start;
    while not (Queue.is_empty queue) do
      let from = Queue.dequeue_exn queue in
      let tos = Hashtbl.find_all edges from in
      List.iter
        (fun to' ->
          match Hashtbl.find_opt used to' with
          | None ->
            Hashtbl.add used to' true;
            Queue.enqueue queue to'
          | Some _ -> ()
        )
        tos
    done;
    Hashtbl.fold (fun k _ l -> k::l) used []
end

let min_of_list =
  List.fold_left (fun a m -> if a < m then a else m) max_int
let take_n n l =
  List.fold_left (fun (ll, i) e -> if i < n then (e::ll, i + 1) else (ll, i + 1)) ([], 0) l
  |> fst |> List.rev

let get_constant_substitution occurrences parameters =
  let occurrences =
    List.map
      (fun (v, ls) ->
        let min_length =
          min_of_list (List.map List.length ls) in
        v, List.map (take_n min_length) ls
      )
      occurrences
  in
  let op =
    List.combine occurrences parameters
    |> List.map (fun ((p, lss), (p', par)) ->
      assert (
        (p.Id.name = dummy_entry_name && p'.Id.name = dummy_entry_name) ||
        Id.eq p p');
      let l_len =
        match lss with
        | [] -> 0
        | x::_ -> List.length x in
      (p, (lss, take_n l_len par))
    ) in
  let all_params =
    List.fold_left
      (fun set (_, (_, params)) ->
        List.fold_left
          (fun s p -> IdSet.add s p)
          set
          params
      )
      IdSet.empty
      op in
  let to_node arg =
    Some (Var {arg with ty=Type.TyBool ()}) in
  let g = Mygraph2.init () in
  List.iter
    (fun (_p, (lss, args)) ->
      List.iter
        (fun ls ->
          List.combine ls (take_n (List.length ls) args)
          |> List.iter
            (fun (form, arg) ->
              match form with
              | Var x -> begin
                if IdSet.exists all_params ~f:(Id.eq x) then
                  Mygraph2.add_edge g (to_node arg) (Some (Var x))
                else
                  Mygraph2.add_edge g (to_node arg) None
              end
              | _ -> begin
                if IdSet.is_empty (fvs form) then
                  Mygraph2.add_edge g (to_node arg) (Some form)
                else
                  Mygraph2.add_edge g (to_node arg) None
              end
            )
        )
        lss
    )
    op;
  let results =
    List.map
      (fun (p, (_, params)) ->
        p,
        List.map
          (fun param ->
            let froms = Mygraph2.reachable_nodes_from g (to_node param) in
            if List.exists ((=)None) froms then None
            else begin
              if List.exists (fun a -> a = None) froms
              then None
              else begin
                let froms =
                  List.filter_map
                    (fun a ->
                      match a with
                      | Some (Var _) -> None
                      | Some t -> Some t
                      | None -> assert false)
                    froms in
                match froms with
                | [x] -> Some x
                | [] -> None
                | _ -> None
              end
            end
          )
          params
      )
      op in
  let () =
    log_string "Substitution:";
    List.iter
      (fun (p, params) ->
        log_string @@
          Id.to_string p ^ ": " ^
          (Hflmc2_util.show_list 
            (fun t_opt ->
              match t_opt with
              | None -> "_"
              | Some t -> Print_syntax.show_hflz t
            )
            params)
      )
      results in
  results

let substitute_occurrences hes subst =
  let rec to_argty args = match args with
    | arg::rem ->
      Type.TyArrow (Id.gen arg.Id.ty, to_argty rem)
    | [] -> Type.TyBool ()
  in
  let rec to_abs body args = match args with
    | arg::rem ->
      Abs (arg, to_abs body rem)
    | [] -> body in
  let rules = merge_entry_rule hes in
  let rules =
    List.map
      (fun {var; body = whole_body; fix} ->
        if var.name = dummy_entry_name then
          {var; body = whole_body; fix}
        else begin
          let _, consts = List.find (fun (p, _) -> Id.eq p var) subst in
          let args, body = decompose_abs whole_body in
          let _, body, args_rev =
            List.fold_left
              (fun (i, body, args') arg ->
                let args', body =
                  match List.nth_opt consts i with
                  | Some (Some t) ->
                    args',
                    Trans.Subst.Hflz.hflz (IdMap.singleton arg t) body
                  | Some None | None ->
                    (arg::args'),
                    body
                in
                (i + 1, body, args')
              )
              (0, body, [])
              args in
          let args = List.rev args_rev in
          let var = {var with ty = to_argty args} in
          let whole_body = to_abs body args in
          {var; body = whole_body; fix}
        end
      )
      rules in
  let new_hes_env =
    List.map (fun {var; _} -> var) rules in
  let rec go phi = match phi with
    | App (phi1, phi2) -> begin
      let rec go_ acc phi = match phi with
        | App (p1, p2) ->
          go_ (p2::acc) p1
        | _ -> acc, phi
      in
      let args, p = go_ [] phi in
      match p with
      | Var x when Id.is_pred_name x.name -> (
        let _, consts = List.find (fun (p, _) -> Id.eq p x) subst in
        let args =
          List.filteri
            (fun i _arg ->
              match List.nth_opt consts i with
              | Some (Some _) -> false
              | Some None -> true
              | None -> true
            )
            args
          |> List.map go in
        let rec rev_go_ body args = match args with
          | [] -> body
          | x::xs -> App (rev_go_ body xs, x)
        in
        let x = List.find (Id.eq x) new_hes_env in
        rev_go_ (Var x) (List.rev args)
      )
      | _ ->
        App (go phi1, go phi2)
    end
    | Var _ | Arith _ | Pred _ | Bool _ -> phi
    | And (p1, p2) ->
      And (go p1, go p2)
    | Or (p1, p2) ->
      Or (go p1, go p2)
    | Abs (x, p) -> Abs (x, go p)
    | Forall (x, p) -> Forall (x, go p)
    | Exists (x, p) -> Exists (x, go p)
  in
  let rules =
    List.map
      (fun rule ->
        let body = go rule.body in
        { rule with body }
      )
      rules in
  let rules, _ =
    Hflz_util.assign_unique_variable_id rules in
  let () =
    log_string "Substed";
    log_string @@ Print_syntax.show_hes ~readable:true rules in
  decompose_entry_rule rules
  
let run hes =
  let occurrences = get_occurrences hes in
  let parameters =
    merge_entry_rule hes
    |> List.map
      (fun {var; body; _} ->
        let params, _ = decompose_abs body in
        (var, params)
      )
    in
  let subst = get_constant_substitution occurrences parameters in
  let hes = substitute_occurrences hes subst in
  hes
  