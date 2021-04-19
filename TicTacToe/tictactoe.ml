type board =
   | NoMark
   | MarkX
   | MarkO;;

let string_of_mark = function
  | NoMark -> "_"
  | MarkX -> "X"
  | MarkO -> "O";;

let board = Array.make_matrix 3 3 NoMark;;

let get_row i board =
   Array.get board i;;

let get_column j board =
   Array.mapi (fun i row -> row.(j)) board;;

let get_first_diagonal board =
   Array.mapi (fun i row -> row.(i)) board;;

let get_second_diagonal board =
   Array.mapi (fun i row -> row.((Array.length row) - i - 1)) board;;

let all_equal_to mark = fun row ->
   Array.fold_left (fun a x -> a && (x = mark)) true row;;

let any_equal_to mark = fun row ->
   Array.fold_left (fun a x -> a || (x = mark)) false row;;

let any_row func = fun board ->
   Array.fold_left (fun a row -> a || func row) false board;;

let get_columns board =
   Array.mapi (fun j x -> get_column j board) board.(0);;

let any_column func = fun board ->
   any_row func (get_columns board);;

let any_diagonal func = fun board ->
   func (get_first_diagonal board) ||
   func (get_second_diagonal board);;

let occupied i j = fun board ->
   board.(i).(j) != NoMark;;

let available board =
   let maybe_available =
      Array.mapi (fun i row ->
         Array.mapi (fun j x -> 
            if x = NoMark then Some(i,j)
            else None) row) in
   Array.fold_left (fun a row ->
      a @ (Array.fold_left (fun b x -> 
         match x with
            | Some(i,j) -> (i,j) :: b
            | None -> b) [] row)) []
      (maybe_available board);;

let tick i j = fun mark -> fun board ->
   Array.mapi (fun _i row ->
      if _i = i then 
         Array.mapi (fun _j x -> 
            if _j = j
                  then mark 
                  else x) row
            else row
         ) board;;

let check mark = fun board ->
   let is_winner = all_equal_to mark in
   any_row is_winner board ||
   any_column is_winner board ||
   any_diagonal is_winner board;;

let winner board =
   if (check MarkX board) then
      MarkX
   else if (check MarkO board) then
      MarkO
   else
      NoMark;;

let stuck board =
   let has_available = any_equal_to NoMark in
   not (any_row has_available board);;

let show board =
   for i = 0 to 2 do
      let v = (Array.map string_of_mark board.(i)) in
      print_endline (String.concat "|" (Array.to_list v))
   done;;

let example () =
   let x i j = tick i j MarkX in
   let o i j = tick i j MarkO in
   let play = (x 0 1 (o 1 2(x 2 1(o 2 2(x 1 1 (board)))))) in
   show play;
   match (winner play) with
      | MarkX -> print_endline "X won"
      | MarkO -> print_endline "O won"
      | _ -> print_endline "No winner";;

class tic_tac_toe =
   object(self)
      val mutable game = ([] : board array array list)
      val mutable turns = ([] : board list)
      method play i j =
         game <- (tick i j self#turn self#recent) :: game;
         turns <- self#turn :: turns
      method undo () =
         ignore (game <- match game with
            | h :: t -> t
            | [] as x -> x);
         turns <- match turns with
            | h :: t -> t
            | [] as x -> x
      method recent =
         match game with
            | h :: t -> h
            | [] -> board
      method turn =
         match turns with
            | MarkX :: t -> MarkO
            | _ -> MarkX
      method previous_turn =
         match turns with
            | h :: t -> h
            | _ -> NoMark
   end;;

let example2 () =
   let game = new tic_tac_toe in

   print_endline "Starting new game...";

   while ((winner game#recent) = NoMark) && (not (stuck game#recent)) do
      let a = available game#recent in
      let rec len l =
         match l with
            | h :: t -> 1 + (len t)
            | [] -> 0 in
      let rec get n l =
         match l with
            | h :: t -> if n = 0 then h else get (n-1) t
            | [] -> raise Not_found in
      let n = len a in
      let k = Random.int (n + 1) in
      if (k = n) then (
         print_endline (String.concat ": " ["Undoing move of player";
         (string_of_mark game#previous_turn)]);
         game#undo ()
      )
      else (
         print_endline (String.concat ": " ["Turn of player"; (string_of_mark game#turn)]);
         let s = get k a in
         match s with
            | (i,j) -> game#play i j
      );
      show game#recent;
      print_endline "";
   done;;

example ();;

example2 ();;


