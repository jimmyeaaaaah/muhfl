let () =
  let rec f x =
    let rec g y = if y=0 then f (x+1) else g (y-1)
    in g x
  in f 0
