module T = Add_arguments_definition
open Hflmc2_syntax

let log_src = Logs.Src.create "Pa_tuple"
module Log = (val Logs.src_log @@ log_src)

let log_string = Hflz_util.log_string Log.info
let log_warn = Hflz_util.log_string Log.warn

type 'ty thflz2 =
  | Bool   of bool
  | Var    of 'ty Id.t
  | Or     of 'ty thflz2 * 'ty thflz2
  | And    of 'ty thflz2 * 'ty thflz2
  | Abs    of ('ty Id.t list) * 'ty thflz2 * 'ty
  | Forall of 'ty Id.t * 'ty thflz2
  | Exists of 'ty Id.t * 'ty thflz2
  | App    of 'ty thflz2 * ('ty thflz2 list)
  | Arith  of 'ty T.tarith
  | Pred   of Formula.pred * 'ty T.tarith list
  [@@deriving eq,ord,show]

type ptype2 = TInt | TBool | TFunc of (ptype2 * T.use_flag) list * ptype2 | TVar of unit Id.t
  [@@deriving eq,ord]

let show_list f ls = "[" ^ (List.map f ls |> String.concat "; ") ^ "]"

let rec pp_ptype2 prec ppf ty =
  match ty with
  | TBool ->
    Fmt.pf ppf "bool"
  | TInt ->
    Fmt.pf ppf "int"
  | TFunc (tts, argty) ->
    if List.hd tts |> snd <> T.dummy_use_flag then
      Print.show_paren (prec > Print.Prec.arrow) ppf "@[<1>%a ->@ %a@]"
        pp_ptype2_arg
        tts
        (pp_ptype2 Print.Prec.arrow) argty
    else
      Print.show_paren (prec > Print.Prec.arrow) ppf "@[<1>%s ->@ %a@]"
        (show_list (Hflmc2_util.fmt_string (pp_ptype2 Print.Prec.zero)) (List.map fst tts))
        (pp_ptype2 Print.Prec.arrow) argty
    | TVar (id) ->
      Fmt.pf ppf "%s" (Id.to_string id)
and pp_ptype2_arg ppf tts =
  let fmt (formatter : Format.formatter) (ty, tag) =
    Print.fprintf
      formatter
      "{%s,%s}"
      (Hflmc2_util.fmt_string (pp_ptype2 Print.Prec.zero) ty)
      (T.show_use_flag tag) in
  let sep ppf () = Fmt.pf ppf "," in
  Fmt.pf ppf "[%a]" (Fmt.list ~sep fmt) tts

let show_ptype2 = Hflmc2_util.fmt_string (pp_ptype2 Print.Prec.zero)

type 'a thes_rule = {var: 'a Id.t; body: 'a thflz2; fix: T.fixpoint}
[@@deriving eq,ord,show]

type 'a thes_rule_in_out = {var_in_out: 'a T.in_out Id.t; body: 'a thflz2; fix: T.fixpoint}
[@@deriving eq,ord,show]

let get_tag = function
  | T.TFunc (_, _, t) -> t
  | _ -> assert false

let get_free_variables phi =
  let rec go phi = match phi with
    | Bool _ -> []
    | Var v -> [v]
    | Or (p1, p2) -> go p1 @ go p2
    | And (p1, p2) -> go p1 @ go p2
    | Abs (xs, p, _) -> List.filter (fun v -> not @@ List.exists (fun x' -> Id.eq x' v) xs) (go p)
    | Forall (x, p) -> List.filter (fun v -> not @@ Id.eq x v) (go p)
    | Exists (x, p) -> List.filter (fun v -> not @@ Id.eq x v) (go p)
    | App (p1, p2) -> go p1 @ (List.map go p2 |> List.flatten)
    | Arith a -> go_arith a
    | Pred (_, ps) -> List.map go_arith ps |> List.concat
  and go_arith a = match a with
    | Int _ -> []
    | Var v -> [v]
    | Op (_, ps) -> List.map go_arith ps |> List.concat
  in
  go phi

let rec get_thflz2_type_without_check phi =
  match phi with
  | Bool _ -> TBool
  | Var v -> v.ty
  | Or (_, _) -> TBool
  | And (_, _) -> TBool
  | Abs (_, _, ty) -> ty
  | Forall (_, p) -> get_thflz2_type_without_check p
  | Exists (_, p) -> get_thflz2_type_without_check p
  | App (p1, _) -> begin
    match get_thflz2_type_without_check p1 with
    | TFunc (_, bodyty) ->
      bodyty
    | _ -> assert false
  end
  | Arith _ -> TInt
  | Pred _ -> TBool

let get_args phi =
  let rec go phi = match phi with
    | Abs (x, p, ty) -> begin
      let argty =
        match ty with
        | TFunc (argty, _) -> argty
        | _ -> assert false in
      let xs, r = go p in
      (x, argty) :: xs, r
    end
    | _ -> [], phi
  in
  go phi

let show_fixpoint = function
  | T.Least -> "μ"
  | Greatest -> "ν"

let thflz_to_ptype = get_thflz2_type_without_check

module Print_temp = struct
  open Print
  open T.Print_temp
  
  let rec hflz_
    (get_type : 'pty thflz2 -> 'pty)
    (pp_t_arg : formatter -> ('pty * Add_arguments_definition.use_flag) list -> unit)
    (format_ty_ : Prec.t -> 'pty Fmt.t)
    (prec : Prec.t)
    (ppf : formatter)
    (phi : 'pty thflz2) = match phi with
      | Bool true -> Fmt.string ppf "true"
      | Bool false -> Fmt.string ppf "false"
      | Var x ->
        (* if !output_debug_info then
          Fmt.pf ppf "(%a : %a)" id x (format_ty_ Prec.zero) x.ty
        else *)
        Fmt.pf ppf "(%a : %a)" id x (format_ty_  Prec.zero) x.ty
      | Or (phi1,phi2)  ->
          (* p_id ppf sid;  *)
          show_paren (prec > Prec.or_) ppf "@[<hv 0>%a@ \\/ %a@]"
            (hflz_ get_type pp_t_arg format_ty_ Prec.or_) phi1
            (hflz_ get_type pp_t_arg format_ty_ Prec.or_) phi2
      | And (phi1,phi2)  ->
          (* p_id ppf sid;  *)
          show_paren (prec > Prec.and_) ppf "@[<hv 0>%a@ /\\ %a@]"
            (hflz_ get_type pp_t_arg format_ty_ Prec.and_) phi1
            (hflz_ get_type pp_t_arg format_ty_ Prec.and_) phi2
      | Abs (xs, psi, ty) -> begin
          (* let f_str = show_flag ty in
          if !output_debug_info then
            show_paren (prec > Prec.abs) ppf "@[<1>(λ%a:{%s}%a.@,%a){%a}@]"
              id x
              f_str
              (format_ty_ Prec.(succ arrow)) x.ty
              (hflz_ get_type show_flag format_ty_ Prec.abs) psi
              (format_ty_ Prec.(succ arrow)) ty
          else begin *)
            (* if !show_tag_as_separator then
              show_paren (prec > Prec.abs) ppf "@[<1>λ%a:%a{%s}.@,%a@]"
                id x
                (format_ty_ Prec.(succ arrow)) x.ty
                f_str
                (hflz_ get_type show_flag format_ty_ Prec.abs) psi
            else *)
            match ty with
            | TFunc (tys, _) -> begin
              if List.hd tys |> snd <> T.dummy_use_flag then
                show_paren (prec > Prec.abs) ppf "@[<1>λ%s:%a.@,%a@]"
                  (show_list Id.to_string xs)
                  pp_t_arg tys
                  (hflz_ get_type pp_t_arg format_ty_ Prec.abs) psi
              else
                show_paren (prec > Prec.abs) ppf "@[<1>λ%s:%s.@,%a@]"
                  (show_list Id.to_string xs)
                  (show_list (Hflmc2_util.fmt_string (pp_ptype2 Print.Prec.zero)) (List.map fst tys))
                  (hflz_ get_type pp_t_arg format_ty_ Prec.abs) psi
            end
            | _ -> assert false
          (* end *)
      end
      | Forall (x, psi) ->
          show_paren (prec > Prec.abs) ppf "@[<1>∀%a.@,%a@]"
            id x
            (hflz_ get_type pp_t_arg format_ty_ Prec.abs) psi
      | Exists (x, psi) ->
          show_paren (prec > Prec.abs) ppf "@[<1>∃%a.@,%a@]"
            id x
            (hflz_ get_type pp_t_arg format_ty_ Prec.abs) psi
      | App (psi1, psi2) -> begin
          (* let ty = get_type psi1 in *)
          (* let f_str = show_flag ty in
          if !show_tag_as_separator then
            show_paren (prec > Prec.app) ppf "@[<1>%a@ %a{%s}@]"
              (hflz_ get_type show_flag format_ty_ Prec.app) psi1
              (hflz_ get_type show_flag format_ty_ Prec.(succ app)) psi2
              f_str
          else *)
            show_paren (prec > Prec.app) ppf "@[<1>%a@ %s@]"
              (hflz_ get_type pp_t_arg format_ty_ Prec.app) psi1
              (show_list (Hflmc2_util.fmt_string (hflz_ get_type pp_t_arg format_ty_ Prec.zero)) psi2)
      end
      | Arith a ->
        arith_ prec ppf a
      | Pred (pred', [f1; f2]) ->
          (* p_id ppf sid;  *)
          Fmt.pf ppf "@[<1>%a@ %a@ %a@]"
            (arith_ prec) f1
            pred pred'
            (arith_ prec) f2
      | Pred _ -> assert false

  let hflz_ : ('pty thflz2 -> 'pty) -> (Prec.t -> 'ty Fmt.t) -> 'ty thflz2 Fmt.t =
    fun get_type format_ty_ -> hflz_ get_type pp_ptype2_arg format_ty_ Prec.zero

  let hflz : (Prec.t -> 'ty Fmt.t) -> 'ty thflz2 Fmt.t =
    hflz_ get_thflz2_type_without_check
  
  let hflz_hes_rule : (Prec.t -> 'ty Fmt.t) -> 'ty thes_rule Fmt.t =
    fun format_ty_ ppf {var; body; fix} ->
      let args, body = get_args body in
      Fmt.pf ppf "@[<2>%s %a =%s@ %a@]"
        (Id.to_string var)
        (pp_print_list
          ~pp_sep:Print_syntax.PrintUtil.fprint_space
          (fun ppf (args, tts) ->
            (* if !show_tag_as_separator then
              fprintf ppf "(%s : (%a){%s})" (Id.to_string arg) (format_ty_ Prec.zero) arg.Id.ty (show_use_flag f2)
            else *)
              fprintf ppf "(%s : %s)"
                (show_list Id.to_string args)
                (Hflmc2_util.fmt_string pp_ptype2_arg tts)
              (* fprintf ppf "(%s : %a)" (Id.to_string arg) (format_ty_ Prec.zero) arg.Id.ty *)
          )
        )
        args
        (show_fixpoint fix)
        (hflz_ get_thflz2_type_without_check format_ty_) body

  let hflz_hes :  (Prec.t -> 'ty Fmt.t) -> 'ty thes_rule list Fmt.t =
    fun format_ty_ ppf rules ->
      Fmt.pf ppf "@[<v>%a@]"
        (Fmt.list (hflz_hes_rule format_ty_)) rules
  
  let hflz_hes_rule_in_out : (Prec.t -> 'ty Fmt.t) -> 'ty thes_rule_in_out Fmt.t =
    fun format_ty_ ppf {var_in_out; body; fix} ->
      let args, body = get_args body in
      Fmt.pf ppf "@[<2>%s %a =%s@ %a@]"
        (Id.to_string var_in_out)
        (pp_print_list
          ~pp_sep:Print_syntax.PrintUtil.fprint_space
          (fun ppf (args, tts) ->
            (* if !show_tag_as_separator then
              fprintf ppf "(%s : (%a){%s})" (Id.to_string arg) (format_ty_ Prec.zero) arg.Id.ty (show_use_flag f2)
            else *)
              fprintf ppf "(%s : %s)"
                (show_list Id.to_string args)
                (Hflmc2_util.fmt_string pp_ptype2_arg tts)
              (* fprintf ppf "(%s : %a)" (Id.to_string arg) (format_ty_ Prec.zero) arg.Id.ty *)
          )
        )
        args
        (show_fixpoint fix)
        (hflz_ get_thflz2_type_without_check format_ty_) body

  let hflz_hes_in_out :  (Prec.t -> 'ty Fmt.t) -> 'ty thes_rule_in_out list Fmt.t =
    fun format_ty_ ppf rules ->
      Fmt.pf ppf "@[<v>%a@]"
        (Fmt.list (hflz_hes_rule_in_out format_ty_)) rules
        
end

let rec convert_ty ty =
  match ty with
  | T.TFunc _ -> begin
    let rec go argtys ty = match ty with
      | T.TFunc (argty, bodyty, t) -> begin
        let argtys = argty::argtys in
        match t with
        | TUse ->
          let argtys = List.map (fun argty -> convert_ty argty, T.dummy_use_flag) argtys |> List.rev in 
          let bodyty = convert_ty bodyty in
          TFunc (argtys, bodyty)
        | TNotUse ->
          go argtys bodyty
        | EFVar _ -> assert false
      end
      (* TODO: 型が未確定のときの処理 *)
      (* | _ -> assert false *)
      | T.TBool ->
        log_warn "conver_ty: tbool (no applications?)";
        let argtys = List.map (fun argty -> convert_ty argty, T.dummy_use_flag) argtys |> List.rev in 
        let bodyty = convert_ty ty in
        TFunc (argtys, bodyty)
      | T.TVar _ ->
        failwith "conver_ty: tvar (not used variable?)"
        (* ; let argtys = List.map (fun argty -> convert_ty argty, T.dummy_use_flag) argtys |> List.rev in 
        let bodyty = convert_ty ty in
        TFunc (argtys, bodyty) *)
      | T.TInt -> failwith "convert_ty: TInt"
    in
    go [] ty
  end
  | TBool -> TBool
  | TInt -> TInt
  | TVar _ -> assert false

let to_thflz2 rules =
  let convert_v_ty v = {v with Id.ty=convert_ty v.Id.ty} in
  let rec to_thflz2_sub phi : ptype2 thflz2 =
    match phi with
    | T.App _ -> begin
      let conv p1 apps =
        let p1 = to_thflz2_sub p1 in
        let apps = List.map to_thflz2_sub apps in
        App (p1, apps)
      in
      let rec go apps phi = match phi with
        | T.App (p1, p2) -> begin
          (* let apps = p2::apps in *)
          let ty1 = T.get_thflz_type_without_check p1 in
          match ty1 with
          | T.TFunc (_, _, t) -> begin
            match t with
            | TUse -> begin
              match apps with
              | [] ->
                go [p2] p1
              | _ ->
                conv phi apps
            end
            | TNotUse ->
              go (p2::apps) p1
            | _ -> assert false
          end
          | _ -> assert false
        end
        | _ ->
          conv phi apps
      in
      go [] phi
    end
    | Abs (_, _, ty_abs) -> begin
      let rec go args phi = match phi with
        | T.Abs (x, p, ty) -> begin
          let args = (convert_v_ty x)::args in
          match ty with
          | T.TFunc (_, _, t) -> begin
            match t with
            | TUse ->
              let p = to_thflz2_sub p in
              Abs (List.rev args, p, convert_ty ty_abs)
            | TNotUse ->
              go args p
            | EFVar _ -> assert false
          end
          | _ -> assert false
        end
        | _ ->
          let p = to_thflz2_sub phi in
          log_string "ptype";
          log_string @@ T.show_ptype ty_abs;
          let ty_abs = convert_ty ty_abs in
          log_string "ptype2";
          log_string @@ show_ptype2 ty_abs;
          Abs (List.rev args, p, ty_abs)
      in
      go [] phi
    end
    | Bool b -> Bool b
    | Var v -> Var (convert_v_ty v)
    | Or (p1, p2) -> Or (to_thflz2_sub p1, to_thflz2_sub p2)
    | And (p1, p2) -> And (to_thflz2_sub p1, to_thflz2_sub p2)
    | Forall (x, p) -> Forall (convert_v_ty x, to_thflz2_sub p)
    | Exists (x, p) -> Exists (convert_v_ty x, to_thflz2_sub p)
    | Arith a -> Arith (go_arith a)
    | Pred (op, ps) -> Pred (op, List.map go_arith ps)
  and go_arith a = match a with
    | Int i -> Int i
    | Var v -> Var (convert_v_ty v)
    | Op (op, ps) -> Op (op, List.map go_arith ps)
  in
  let rules =
    List.map
      (fun {T.var; body; fix} ->
        let body = to_thflz2_sub body in
        {var = {var with ty = convert_ty var.ty.inner_ty}; body; fix}
      )
      rules in
  (* log_string "to_thflz2.tuple";
  log_string @@
    Hflmc2_util.fmt_string
      (Print_temp.hflz_hes pp_ptype2) rules; *)
  rules

let check_thflz2_type rules =
  let global_env = List.map (fun {var; _} -> var) rules in
  let rec go env phi = match phi with
    | Bool _ -> TBool
    | Var v -> begin
      match List.find_opt (fun v' -> Id.eq v' v) env with
      | Some v' ->
        (* print_endline "v'";
        print_endline @@ show_ptype2 v'.ty;
        print_endline "v";
        print_endline @@ show_ptype2 v.ty; *)
        assert (v'.ty = v.ty);
        v.ty
      | None -> assert false
    end
    | Or (p1, p2) ->
      assert (go env p1 = TBool);
      assert (go env p2 = TBool);
      TBool
    | And (p1, p2) ->
      assert (go env p1 = TBool);
      assert (go env p2 = TBool);
      TBool
    | Abs (xs, p, fty) -> begin
      (* print_endline "formula";
      print_endline @@ Hflmc2_util.fmt_string (Print_temp.hflz pp_ptype2) phi; *)
      let bodyty =
        (match fty with
        | TFunc (argtys, bodyty) ->
          List.iter
            (fun (x, a) ->
              (* print_endline @@ show_ptype2 x.Id.ty;
              print_endline @@ show_ptype2 a; *)
              assert (x.Id.ty = a)
            )
            (List.combine xs (List.map fst argtys));
          bodyty
        | _ -> assert false) in
      let ty = go (xs @ env) p in
      assert (ty = bodyty);
      TFunc (List.map (fun v -> v.Id.ty, T.dummy_use_flag) xs, ty)
    end
    | Forall (x, body) ->
      go (x::env) body
    | Exists (x, body) ->
      go (x::env) body
    | App (p, ps) -> begin
      let ty1 = go env p in
      match ty1 with
      | TFunc (argtys, bodyty) -> begin
        let tys = List.map (go env) ps in
        (* print_endline "formula";
        print_endline @@ Hflmc2_util.fmt_string (Print_temp.hflz pp_ptype2) phi; *)
        List.iter
          (fun (ty1, ty2) ->
            (* print_endline @@ show_ptype2 ty1;
            print_endline @@ show_ptype2 ty2; *)
            assert (ty1 = ty2)
          )
          (List.combine (List.map fst argtys) tys);
        bodyty
      end
      | _ -> assert false
    end
    | Pred (_, args) ->
      List.iter (fun arg -> go_arith env arg) args;
      TBool
    | Arith a ->
      go_arith env a;
      TInt
  and go_arith env a = match a with
  | Int _ -> ()
  | Var v -> begin
    match List.find_opt (fun v' -> Id.eq v v') env with
    | Some _ ->
      assert (v.ty = TInt)
    | None -> assert false
  end
  | Op (_, args) ->
    List.iter (fun arg -> go_arith env arg) args;  
  in
  List.iter
    (fun {var; body; _} ->
      let ty = go global_env body in
      assert (var.Id.ty = ty);
    )
    rules

let rec to_hflz_ty ty =
  match ty with
  | TBool -> Type.TyBool ()
  | TFunc (argtys, ty) ->
    let argtys = List.map (fun (argty, _) -> to_hflz_argty argty) argtys in
    let ty = to_hflz_ty ty in
    let rec go_argtys bodyty argtys =
      match argtys with
      | ty::tys -> Type.TyArrow ((Id.gen ty), go_argtys bodyty tys)
      | [] -> bodyty
    in
    go_argtys ty argtys
  | _ -> assert false
and to_hflz_argty ty =
  match ty with
  | TInt -> Type.TyInt
  | TVar _ -> assert false
  | _ -> Type.TySigma (to_hflz_ty ty)

let rec to_hflz env body =
  match body with
  | Bool b -> Hflz.Bool b
  | Var v -> begin
    match List.find_all (fun x -> Id.eq x v) env with
    | [v'] ->
      Hflz.Var v'
    | [] -> failwith "to_hflz: not found"
    | _ -> failwith "to_hflz: multiple found"
  end
  | Or (p1, p2) -> Hflz.Or (to_hflz env p1, to_hflz env p2)
  | And (p1, p2) -> Hflz.And (to_hflz env p1, to_hflz env p2)
  | Abs (xs, p, _) ->
    let xs: unit Type.ty Type.arg Id.t list = List.map (fun x -> {x with Id.ty=to_hflz_argty x.Id.ty}) xs in
    let rec go_abs p xs =
      match xs with
      | x::xs -> Hflz.Abs (x, go_abs p xs)
      | [] -> p
    in
    let env = env @
      (List.filter_map (fun x -> match x.Id.ty with Type.TySigma ty -> Some {x with ty} | _ -> None) xs) in
    let p = to_hflz env p in
    go_abs p xs
  | Forall (x, p) ->
    Forall ({x with ty=Type.TyInt}, to_hflz env p)
  | Exists (x, p) ->
    Exists ({x with ty=Type.TyInt}, to_hflz env p)
  | App (p, xs) ->
    let rec go_app p xs =
      match xs with
      | x::xs -> Hflz.App (go_app p xs, x)
      | [] -> p
    in
    let p = to_hflz env p in
    let xs = List.map (to_hflz env) xs in
    go_app p (List.rev xs)
  | Arith a -> Hflz.Arith (go_arith a)
  | Pred (p, as') -> Hflz.Pred (p, List.map go_arith as')
and go_arith a =
  match a with
  | Int i -> Int i
  | Var v -> Var {v with ty=`Int}
  | Op (op, as') -> Op (op, List.map go_arith as')
  
let to_hes rules =
  let pred_vars =
    List.map
      (fun {var; _} -> {var with ty = to_hflz_ty var.ty})
      rules in
  List.mapi
    (fun i {fix; body; var = _} ->
      let body = to_hflz pred_vars body in
      let fix = match fix with
        | Least -> Fixpoint.Least
        | Greatest -> Fixpoint.Greatest in
      let var = List.nth pred_vars i in
      {Hflz.var; fix; body}
    )
    rules
