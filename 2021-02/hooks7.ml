(*
 *  Jane Street Puzzle - Feb 2021    
 *  Willem Hoek <willem@matimba.com> 
 *  Blog Post:  https://whoek.com/b/howto-solve-jane-street-puzzle-feb-2021-hooks7 
 *)

(**** START of HELPER functions *************************************)

(* option_to_int : option int -> int *) 
let  option_to_int = function
  | Some x -> x
  | None -> 0

(* remove consecutive duplicates from a list                              *)
(* https://dev.realworldocaml.org/lists-and-patterns.html#scrollNav-6-1   *)
let rec destutter = function
  | [] | [_] as l -> l
  | hd :: (hd' :: _ as tl) when hd = hd' -> destutter tl
  | hd :: tl -> hd :: destutter tl

(* print matrix : int array array -> unit *)
let print_matrix matrix =
  let dimx = Array.length matrix in
  let dimy = Array.length matrix.(0) in
  for x = 0 to (dimx - 1) do
    for y = (dimy - 1) downto 0 do
      Printf.printf "%i " matrix.(x).(y);
    done;
    Printf.printf "\n%!"
  done;
  Printf.printf "\n%!"

(* convert matrix : option int array array -> int array array *)
let int_of_matrix matrix =
  let dimx = Array.length matrix in
  let dimy = Array.length matrix.(0) in
  let m = Array.make_matrix dimx dimy 0 in
  for x = 0 to (dimx - 1) do
    for y = 0 to (dimy - 1) do
      m.(x).(y) <- option_to_int matrix.(x).(y)
    done
  done;
  m

let rec list_last = function
  | [] -> failwith "List is empty"
  | [x] -> x
  | hd :: tl -> list_last tl

let rec power x y =
  if(y <= 1 ) then x else x * power x (y-1)

(* convert from base 10 to base n where n < 10 : int -> int -> string *)
let rec frombase10 x n =
  let digit = x mod n in
  let div = x / n in
  if div = 0 then string_of_int digit
  else (frombase10 div n) ^ string_of_int digit

(* return right n characters of as string : string -> int -> string *)
let right_str str n  =
  let len = String.length str in
  String.sub str (len - n) n 

(* remove first item with value v from a list *)
let rec remove_val v = function
  | [] -> []
  | h :: t -> if h = v then t else h :: remove_val v t


(**** END of HELPER functions ***************************************)

let cell2xy cell =
  let y = cell / 9 in
  let x = cell mod 9 in
  (x, y)

let prefix_zeros x = right_str ("00000000" ^ x) 8

(* TEST *)
let () = assert ("00001111" = prefix_zeros "1111")

let make_list first last  =  (* n = size of the grid *)
  let base4 x =  frombase10 (x + first) 4 |> prefix_zeros in
  List.init (last - first) base4

let fill_hook grid num (x, y) =
  for i = 0 to 8 do
    if grid.(i).(y) = 1 then grid.(i).(y) <- num;
    if grid.(x).(i) = 1 then grid.(x).(i) <- num
  done;
  grid

let full_grid str =
  let grid = ref (Array.make_matrix 9 9 1) in  (* x y  default_value *)
  let y_bottom = ref 0 in
  let y_top = ref 8 in
  let x_left = ref 0 in
  let x_right = ref 8 in
  let rec add_hook sub_str =
    let len = String.length sub_str in
    if len = 0 then !grid
    else begin
        match sub_str.[0] with
        | '0' -> grid := fill_hook !grid (len + 1) (!x_left, !y_bottom);
                 y_bottom := !y_bottom + 1;
                 x_left := !x_left + 1;
                 add_hook (right_str sub_str (len - 1))
        | '1' -> grid := fill_hook !grid (len + 1) (!x_left, !y_top);
                 y_top := !y_top - 1;
                 x_left := !x_left + 1;
                 add_hook (right_str sub_str (len - 1))
        | '2' -> grid := fill_hook !grid (len + 1) (!x_right, !y_top);
                 y_top := !y_top - 1;
                 x_right := !x_right - 1;
                 add_hook (right_str sub_str (len - 1))
        | '3' -> grid := fill_hook !grid (len + 1) (!x_right, !y_bottom);
                 y_bottom := !y_bottom + 1;
                 x_right := !x_right - 1;
                 add_hook (right_str sub_str (len - 1))
        | _ -> assert false
      end
  in add_hook str

let check54 grid =
  grid.(4).(2) = 5 && grid.(4).(6) = 4 &&
    grid.(4).(0) + grid.(4).(1) = 15 &&
      grid.(4).(7) + grid.(4).(8) = 15 &&
        grid.(7).(2) + grid.(8).(2) = 15

(* All entries for every hook *)
let bin = [|[];[1];[2;2;0];[3;3;3;0;0];[4;4;4;4;0;0;0];[5;5;5;5;5;0;0;0;0];
            [6;6;6;6;6;6;0;0;0;0;0];[7;7;7;7;7;7;7;0;0;0;0;0;0];
            [8;8;8;8;8;8;8;8;0;0;0;0;0;0;0];[9;9;9;9;9;9;9;9;9;0;0;0;0;0;0;0;0]|]

(* get list of possible entries left to place in a hook *)
let[@inlne]  get_population grid full_grid n =
  let p = ref bin.(n)  in
  for x = 0 to 8 do
    for y = 0 to 8 do
      if full_grid.(x).(y) = n && grid.(x).(y) <> None
      then begin
          let to_remove = grid.(x).(y) |> option_to_int in 
          p :=  remove_val to_remove !p
        end
    done
  done;
  !p |> destutter    (* to remove duplucates *)

(* TEST *)
let () =
  let grid = Array.make_matrix 9 9 (Some 9) in
  let full_grid = Array.make_matrix 9 9 1 in
  full_grid.(1).(1) <- 3;
  full_grid.(2).(1) <- 3;
  full_grid.(3).(1) <- 3;
  full_grid.(4).(1) <- 3;
  grid.(1).(1) <- None;
  grid.(2).(1) <- Some 3;
  grid.(3).(1) <- Some 0;
  grid.(4).(1) <- Some 0;    
  assert ([3] = get_population grid full_grid 3);
  assert ([4; 0] = get_population grid full_grid 4)

let is_connected some_grid =
  let (x, y) = (4, 2) in   (* known number *)
  let mark = Array.make_matrix 9 9 true in
  let count = ref 0 in
  let rec flood x y = 
    incr count;
    mark.(x).(y) <- false;
    if y > 0 && option_to_int @@ some_grid.(x).(y-1) <> 0 &&   (* N *)
         mark.(x).(y-1) then flood x (y-1);                   
    if y < 8 && option_to_int @@ some_grid.(x).(y+1) <> 0 &&   (* S *)
         mark.(x).(y+1) then flood x (y+1);                    
    if x > 0 && option_to_int @@ some_grid.(x-1).(y) <> 0 &&   (* W *)
         mark.(x-1).(y) then flood (x-1) y;
    if x < 8 && option_to_int @@ some_grid.(x+1).(y) <> 0 &&   (* E *)
         mark.(x+1).(y) then flood (x+1) y;
  in
  let () = flood x y in
  !count = 45                  (* 1 + 2 + 3 + 4 + 5 + 6 + 7 + 8 + 9 *)

let check_region region option_grid =
  let cval cell =
    let x,y = cell2xy cell in
    let value = option_to_int option_grid.(x).(y) in
    value
  in
  match region with
  | 10 -> 15 = cval 0 + cval 1 + cval 9 + cval 10
  | 13 -> 15 = cval 4 + cval 13
  | 15 -> 15 = cval 5 + cval 6 + cval 14 + cval 15
  | 17 -> 15 = cval 7 + cval 8 + cval 16 + cval 17
  | 26 -> 15 = cval 25 + cval 26
  | 27 -> 15 = cval 18 + cval 19 + cval 27
  | 31 -> 15 = cval 3 + cval 12 + cval 21 + cval 22 + cval 23 + cval 31
  | 37 -> 15 = cval 28 + cval 36 + cval 37
  | 39 -> 15 = cval 2 + cval 11 + cval 20 + cval 29 + cval 30 + cval 39
  | 44 -> 15 = cval 34 + cval 35 + cval 44
  | 51 -> 15 = cval 24 + cval 32 + cval 33 + cval 42 + cval 51
  | 53 -> 15 = cval 43 + cval 52 + cval 53
  | 54 -> 15 = cval 45 + cval 46 + cval 54
  | 65 -> 15 = cval 38 + cval 47 + cval 48 + cval 56 + cval 65
  | 71 -> 15 = cval 41 + cval 50 + cval 59 + cval 60 + cval 61 + cval 62 + cval 71
  | 75 -> 15 = cval 40 + cval 49 + cval 55 + cval 57 + cval 58 + cval 63 + cval 64
               + cval 66 + cval 72 + cval 73 + cval 74 + cval 75
  | 76 -> 15 = cval 67 + cval 76
  | 78 -> 15 = cval 68 + cval 77 + cval 78
  | 80 -> 15 = cval 69 + cval 70 + cval 79 + cval 80 
  | _ -> true

let is_valid option_grid cell x y  =
  check_region cell option_grid
  && (y = 0 || x = 0
      || option_grid.(x).(y) = Some 0
      || option_grid.(x).(y-1) = Some 0
      || option_grid.(x-1).(y) = Some 0
      || option_grid.(x-1).(y-1) = Some 0)

(* recursive backtracking  *)
let rec solve some_grid full_grid cell =
  let (x, y) = cell2xy cell in
  if cell = 81 then begin
      if is_connected some_grid then begin
          print_endline "\nFound a solution!!!!";
          some_grid
          |> int_of_matrix
          |> print_matrix;
          true
        end
      else false
    end
  else if some_grid.(x).(y) <> None then solve some_grid full_grid (cell + 1)
  else
    let n = full_grid.(x).(y) in
    let pop = get_population some_grid full_grid n in
    let check v =
      some_grid.(x).(y) <- Some v;
      (is_valid some_grid cell x y && solve some_grid full_grid (cell + 1)) ||
        (some_grid.(x).(y) <- None; false)
    in
    List.exists (fun z -> check z) pop

let main () = begin
    let hook_list = make_list 0 (power 4 8) in      (*65_536 *)
    let valid_list = List.filter (fun x -> full_grid x |> check54) hook_list in
    Printf.printf "First position: %S\n" (List.hd hook_list);
    Printf.printf "Last position:  %S\n" (list_last hook_list);
    Printf.printf "Total positions: %i\n" (List.length hook_list);
    Printf.printf "Valid positions: %i\n" (List.length valid_list);
    for n = 0 to (List.length valid_list - 1) do
      let full_board = full_grid @@ List.nth valid_list n in 
      let empty_grid =  (Array.make_matrix 9 9 None) in
      empty_grid.(4).(2) <- Some 5; 
      empty_grid.(4).(6) <- Some 4;
      let _ = solve empty_grid full_board 0 in
      Printf.printf ".%!";
    done;
    print_endline "\nAll done!"
  end;;

main ()

