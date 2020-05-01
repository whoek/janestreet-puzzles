(* triangle is marked with 0's, borders with 1's *)
let create_grid size =
  let grid = Array.make_matrix (size + 2) (size + 2) 1 in
  for y = 1 to size  do
    for x = 1 to size do
      if x <= y then grid.(x).(y) <- 0
    done
  done;
  grid

(* find next open space for triad placement *)
let next_dot grid =
  let x' = ref 0 in
  let y' = ref 0 in
  let size = Array.length grid - 2 in
  try
    for x=1 to size do
      for y=1 to size do
        if grid.(x).(y) = 0 then
          begin
            y' := y;
            x' := x;
            raise Exit
          end
      done
    done;
    None
  with Exit -> Some (!x', !y')

type triad = Up | Down

let is_valid x y placement grid =
  assert (grid.(x).(y) = 0); 
  match placement with
  | Up -> grid.(x).(y+1) = 0 && grid.(x+1).(y+1) = 0
  | Down -> grid.(x+1).(y) = 0 && grid.(x+1).(y+1) = 0

(* place or remove the triad *)
let change_grid x y placement grid value =
  match placement with
  | Up -> grid.(x).(y) <- value;
          grid.(x).(y+1) <- value;
          grid.(x+1).(y+1) <- value
  | Down -> grid.(x).(y) <- value;
            grid.(x+1).(y) <- value;
            grid.(x+1).(y+1) <- value

let rec solve grid =
  let dot = next_dot grid in
  match dot with
  | None -> true
  | Some (x, y) ->
     let check placement =
       if is_valid x y placement grid then
         begin
           change_grid x y placement grid 1;
           if solve grid then true else
             begin
               change_grid x y placement grid 0;
               false
             end
         end
       else false in
     List.exists (fun z -> check z) [Up; Down]

let print_header () =
  Format.printf "  N      dots    triads  solve\n%!";
  Format.printf " --   -------   -------  -----\n%!"

let () =
  print_header ();
  for n=1 to 26 do
    let dots = (n + 1) * n / 2 in
    let triads = float_of_int dots /. 3. in
    let result  =
      if (dots mod 3 = 0) && (solve @@ create_grid n)
      then "1" else "-" in
    Format.printf "%3i %9i %9.1f %6s \n%!" n dots triads result
  done