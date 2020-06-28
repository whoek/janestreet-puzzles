let create_grid n =
  (* create matrix with n numbers; y is rows, x is series of numbers *)
  let grid = Array.init n
      (fun y -> Array.init n
          (fun x -> x + y - 1)) in
  (* copy values from left & right of diagonal z to next row y *)
  for z = 3 to n / 2 do
    for x = 1 to z - 2  do
      grid.(x * 2 - 1).(z) <- grid.(z - 1 - x).(z - 1);   (* L *)
      grid.(x * 2).(z) <- grid.(z - 1 + x).(z - 1);       (* R *)
    done
  done;
  grid

let print_grid grid size =
  let min_size = min size (Array.length grid - 1) in
  for y = 1 to min_size  do
    for x = 1 to min_size  do
      Printf.printf "%3i%!" grid.(x).(y)
    done;
    Printf.printf "\n%!"
  done

let print_answer grid size =
  for n = 1 to size do
    for  z = 1 to Array.length grid - 1 do
      if n = grid.(z).(z)  
      then Printf.printf "%5i %5i \n%!" n z
    done
  done

let main () =
  let a = create_grid 1000 in
  print_grid a 9;
  print_string "\n";
  print_answer a 15
  
let () = main ()
