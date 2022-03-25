let rec tarai x y z =
  print_endline @@ "(" ^ string_of_int x ^ "," ^ string_of_int y ^ "," ^ string_of_int z ^ ")";
  if y < x then
    tarai
      (tarai (x-1) y z)
      (tarai (y-1) z x)
      (tarai (z-1) x y)
  else
    y

let () =
  tarai 3 2 1 |> print_int
