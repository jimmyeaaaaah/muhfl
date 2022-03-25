let rec tak x y z =
  print_endline @@ "(" ^ string_of_int x ^ "," ^ string_of_int y ^ "," ^ string_of_int z ^ ")";
  if y < x then
    tak
      (tak (x-1) y z)
      (tak (y-1) z x)
      (tak (z-1) x y)
  else
    z

let main () =
  tak (read_int ()) (read_int ()) (read_int ())
