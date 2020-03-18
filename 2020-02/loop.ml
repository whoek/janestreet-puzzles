(* OCaml program by Willem Hoek, Jane Street Puzzle of Feb / Mar 2020 *)

let hit x y = 
  (x < 0. && x > -1. && y > 0. && y < 1.) ||   
  (x > 1. && x <  2. && y > 0. && y < 1.) || 
  (y < 0. && y > -1. && x > 0. && x < 1.) ||
  (y > 1. && y <  2. && x > 0. && x < 1.)

let print_header () =
  Format.printf "\n"; 
  Format.printf "  length   probability     one_line      segments    xn    yn    sn\n";
  Format.printf " --------- ----------- ------------ ------------- ----- ----- -----\n%!"

let print_stats hit_count total_count length xn yn sn = 
  Format.printf "%10f %11f" length  (float_of_int hit_count /. float_of_int total_count);
  Format.printf "%#13i %#13i %5i %5i %5i\n%!"  hit_count total_count xn yn sn

let pol2cart length sigma = 
  (length *. cos sigma, length *. sin sigma)

let calculate xn yn sn length = 
  let pi2 = 2. *. 3.1415926535897931 in
  let total_count = ref 0 in
  let hit_count = ref 0 in
  for s = 0 to (sn - 1) do
    let sigma = float_of_int s *. pi2 /. float_of_int sn in 
    let dx, dy  = pol2cart length sigma in
    for x = 0 to (xn - 1) do
      let x_start = float_of_int x /. float_of_int xn in
      for y = 0 to (yn - 1) do
        let y_start = float_of_int y /. float_of_int yn in
        let x_end = x_start +. dx in
        let y_end = y_start +. dy in
        if hit x_end y_end then incr hit_count;
        incr total_count
      done;
    done;
  done;
  print_stats !hit_count !total_count length xn yn sn

let () = 
  let length = ref 0.0 in
  let incr = 0.1 in
  print_header ();
  while !length < 2.300001 do     
    calculate 200 200 300 !length;
    length := !length +. incr;
  done
