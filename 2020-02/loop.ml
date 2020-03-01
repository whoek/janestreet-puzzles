(* Ocaml program - Jane Street puzzle, Feb 2020 *)

open Format

let hit x y = 
  (x < 0. && x > -1. && y > 0. && y < 1.) || 
  (x > 1. && x <  2. && y > 0. && y < 1.) || 
  (y < 0. && y > -1. && x > 0. && x < 1.) ||
  (y > 1. && y <  2. && x > 0. && x < 1.)

let pol2cart length sigma = 
  (length *. cos sigma, length *. sin sigma)

let calculate xn yn sn length = 
  let pi2 = 2. *. 3.1415926535897931 in
  let total_count = ref 0 in
  let hit_count = ref 0 in
  for x = 0 to (xn - 1) do
    let x_start = float_of_int x /. float_of_int xn in
    for y = 0 to (yn - 1) do
      let y_start = float_of_int y /. float_of_int yn in
      for s = 0 to (sn - 1) do
        let sigma = float_of_int s *. pi2 /. float_of_int sn in 
        let dx, dy  = pol2cart length sigma in
        let x_end = x_start +. dx in
        let y_end = y_start +. dy in
        if hit x_end y_end then incr hit_count;
        incr total_count
      done;
    done;
  done;
  print_stats !hit_count !total_count length xn yn sn

let print_header () = 
  printf "\nlength\t\tprobability\thit\tcount\tx\ty\ts\n%!";
  printf "------\t\t-----------\t---\t-----\t-\t-\t-\n%!"

let print_stats hit_count total_count length xn yn sn = 
  printf "%f\t%f" length  (float_of_int hit_count /. float_of_int total_count);
  printf "\t%d\t%d\t%d\t%d\t%d\t\n%!"  hit_count total_count xn yn sn

let () = 
  let length = ref 0.0 in
  let incr = 0.05 in
  print_header ();
  while !length < 2.5 do     
    calculate 200 200 200 !length;
    length := !length +. incr;
  done
