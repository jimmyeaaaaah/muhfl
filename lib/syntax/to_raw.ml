module R = Raw_hflz
module H = Hflz
module A = Arith

let convert_hflz without_id phi =
  let to_string id = Id.to_string ?without_id id in
  let rec go phi =
    match phi with
    | H.Bool b -> R.Bool b
    | H.Var v -> R.Var (to_string v)
    | H.Or (p1, p2) -> R.Or (go p1, go p2)
    | H.And (p1, p2) -> R.And (go p1, go p2)
    | H.Abs (x, p) -> R.Abs (to_string x, go p)
    | H.App (p1, p2) -> R.App (go p1, go p2)
    | H.Forall (x, p) -> R.Forall (to_string x, go p)
    | H.Exists (x, p) -> R.Exists (to_string x, go p)
    | H.Pred (p, xs) -> R.Pred (p, List.map go_arith xs)
    | H.Arith a -> go_arith a
  and go_arith a = match a with
    | A.Int i -> R.Int i
    | A.Op (op, xs) -> R.Op (op, List.map go_arith xs)
    | A.Var v -> R.Var (to_string v)
  in
  go phi
    
let convert ?without_id (hes : 'a H.hes) : R.hes =
  Hflz.merge_entry_rule hes
  |> List.map
    (fun {H.var; fix; body} ->
      let args, body = Hflz.decompose_abs body in
      let body = convert_hflz without_id body in
      {
        R.var = Id.to_string ?without_id var;
        args = List.map (Id.to_string ?without_id) args;
        fix = fix;
        body = body
      }
    )
