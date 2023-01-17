type move = Up | Down | Left | Right

let roll_dice  (top, bottom, up, down, left, right) d =
  match d with
  | Up    -> down,  up,    top,    bottom, left,   right
  | Down  -> up,    down,  bottom, top,    left,   right
  | Right -> left,  right, up,     down,   bottom, top
  | Left  -> right, left,  up,     down,   top,    bottom

let next_cell cell move =
  match move with
  | Up -> cell + 6
  | Down -> cell - 6
  | Left -> cell - 1
  | Right -> cell + 1

let valid_moves cell  =
  (
    match cell with
    | 0 |  6 | 12 | 18 | 24 | 30 -> [Right]
    | 5 | 11 | 17 | 23 | 29 | 35 -> [Left]
    | _ -> [Right; Left]
  ) @ (
    match cell with
    |  0 |  1 |  2 |  3 |  4 | 5  -> [Up]
    | 30 | 31 | 32 | 33 | 34 | 35 -> [Down]
    | _ -> [Up; Down]
  )

let grid =
  [|
    0.  ;  77.;  32.; 403.; 337.; 452.;
    5.  ;  23.;  -4.; 592.; 445.; 620.;
    -7. ;   2.; 357.; 452.; 317.; 395.;
    186.;  42.; 195.; 704.; 452.; 228.;
    81. ; 123.; 240.; 443.; 353.; 508.;
    57. ;  33.; 132.; 268.; 492.; 732.;
  |]

let print_dice dice =
  let a, b, c, d, e, f = dice in
  Printf.printf "dice = top:%g  bottom:%g  up:%g  down:%g  left:%g  right:%g\n"
    a b c d e f

let print_int_lst p =
  List.iter (Printf.printf "%i ") p;
  Printf.printf "\n"

let unvisited all visited =
  List.filter (fun x -> not @@ List.mem x visited) all

let () = assert (unvisited [1;2;3;4] [1;3] = [2;4])

let sum_values lst  =
  let f acc cell = acc +. grid.(cell) in
  List.fold_left f 0.0 lst

let () = assert (sum_values [6; 35] = 737.0)

let range i = List.init i succ

let () = assert (range 3 = [1; 2; 3])

let print_solution ~new_dice ~cell ~path =
  let all = range 35 in
  let unvisited_cells = unvisited all @@ cell :: path in
  let sum_of_values = sum_values unvisited_cells in
  print_dice  new_dice;
  Printf.printf "unvisited cells = ";
  print_int_lst  unvisited_cells;
  Printf.printf "Sum of values in the unvisited cells = %g\n"
    sum_of_values

let rec solve ~dice ~cell ~n ~path ~score  =
  print_int_lst @@ List.rev @@ cell :: path;
  let new_score = grid.(cell) in
  let new_top = (
      if n = 0.0 then 0.
      else  (new_score -. score) /. n
    ) in
  let top, d2, d3, d4, d5, d6 = dice in
  let new_dice = new_top, d2, d3, d4, d5, d6 in
  let valid = (top = new_top || top = 0.) in
  if cell = 35 && valid then begin
      Printf.printf "Solution found!!!\n";
      print_solution ~new_dice ~cell ~path;
      true                    (* stop looking *)
    end
  else if valid then begin
      let moves = valid_moves cell in
      let check move =
        let new_cell = next_cell cell move in
        solve ~dice:(roll_dice new_dice move)
          ~cell:new_cell
          ~n:(n +. 1.)
          ~path:(cell :: path)
          ~score:new_score
      in
      List.exists check moves
    end
  else false                (* backtrack *)

let main () =
  let dice = 0.0,  0.0,  0.0,  0.0,  0.0,  0.0 in
  let _ = solve ~dice ~cell:0 ~n:0.0 ~path:[] ~score:0.0 in
  Printf.printf "Cheers!\n"

let () = main ()