open Core
open Angstrom

type color = Red | Blue | Green [@@deriving sexp_of]

(** a hand contains a set of dice of red, green, or blue *)
type hand = {red: int; blue: int; green: int} [@@deriving sexp_of]

type game = {id: int; hands: hand list} [@@deriving sexp_of]

(* parse a string and output a game. example inputs:
   Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green would return a game with id 1 and hands
   [ {red = 4; blue = 3; green = 0}; {red = 1; blue = 6; green = 2}; {red = 0; blue = 0; green = 2} ]
   Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue would return a game with id 2 and hands
   [ {red = 0; blue = 1; green = 2}; {red = 1; blue = 4; green = 3}; {red = 0; blue = 1; green = 1} ]
   Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
   Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
   Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green*)

module Parser = struct
  let is_digit = function '0' .. '9' -> true | _ -> false

  let is_whitespace = function ' ' | '\t' | '\n' -> true | _ -> false

  let integer = take_while1 is_digit >>| int_of_string

  let whitespace = skip_while is_whitespace

  let color =
    string "red"
    >>| (fun _ -> Red)
    <|> (string "blue" >>| fun _ -> Blue)
    <|> (string "green" >>| fun _ -> Green)

  let dice_count =
    whitespace *> integer
    >>= fun n -> whitespace *> color >>= fun c -> return (n, c)

  let hand =
    sep_by1 (string ", ") dice_count
    >>= fun dice_counts ->
    let empty_hand = {red= 0; blue= 0; green= 0} in
    return
      (List.fold dice_counts ~init:empty_hand ~f:(fun hand (n, c) ->
           match c with
           | Red ->
               {hand with red= n}
           | Blue ->
               {hand with blue= n}
           | Green ->
               {hand with green= n} ) )

  let hands = sep_by1 (string "; ") hand >>= fun hands -> return hands

  let game =
    whitespace *> string "Game " *> integer
    >>= fun id -> string ": " *> hands >>= fun hands -> return {id; hands}

  let hand_of_string s =
    Angstrom.parse_string hand s ~consume:All |> Result.ok_or_failwith

  let%expect_test "game_of_string" =
    " 1 blue, 2 green" |> hand_of_string |> [%sexp_of: hand] |> print_s ;
    [%expect {| ((red 0) (blue 1) (green 2)) |}]

  let%expect_test "multiple hands" =
    "1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue"
    |> Angstrom.parse_string hands ~consume:All
    |> Result.ok_or_failwith |> [%sexp_of: hand list] |> print_s ;
    [%expect
      {|
    (((red 0) (blue 1) (green 2)) ((red 1) (blue 4) (green 3))
     ((red 0) (blue 1) (green 1))) |}]

  let game_of_string s =
    Angstrom.parse_string game s ~consume:All |> Result.ok_or_failwith

  let%expect_test "game_of_string" =
    "Game 11: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green" |> game_of_string
    |> [%sexp_of: game] |> print_s ;
    [%expect
      {|
    ((id 11)
     (hands
      (((red 4) (blue 3) (green 0)) ((red 1) (blue 6) (green 2))
       ((red 0) (blue 0) (green 2))))) |}]
end

let max_dice (game : game) : hand =
  (* loop over the hands and find the max number of dice for each color *)
  let tally = {red= 0; blue= 0; green= 0} in
  List.fold game.hands ~init:tally ~f:(fun max_dice hand ->
      { red= max max_dice.red hand.red
      ; blue= max max_dice.blue hand.blue
      ; green= max max_dice.green hand.green } )

let%expect_test "max dice`" =
  "Game 11: 3 blue, 4 red; 1 red, 12 green, 6 blue; 2 green"
  |> Parser.game_of_string |> max_dice |> [%sexp_of: hand] |> print_s ;
  [%expect {| ((red 4) (blue 6) (green 12)) |}]

(** a game is possible if all of its hands have dice less than or equal to the given max dice*)
let possible game max_dice =
  List.for_all game.hands ~f:(fun hand ->
      hand.red <= max_dice.red && hand.blue <= max_dice.blue
      && hand.green <= max_dice.green )

let possible_game_ids max_dice game_input =
  List.map game_input ~f:Parser.game_of_string
  |> List.filter ~f:(fun game -> possible game max_dice)
  |> List.map ~f:(fun game -> game.id)

let sample_input =
  [ "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green"
  ; "Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue"
  ; "Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red"
  ; "Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red"
  ; "Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green" ]

let%expect_test "possible_game_ids" =
  let max_dice = {red= 12; green= 13; blue= 14} in
  possible_game_ids max_dice sample_input |> [%sexp_of: int list] |> print_s ;
  [%expect {| (1 2 5) |}]

(** sum up the ids of the possible games*)
let part_1 () =
  let input = In_channel.read_lines "./inputs/2.txt" in
  let max_dice = {red= 12; green= 13; blue= 14} in
  possible_game_ids max_dice input
  |> List.sum (module Int) ~f:(fun x -> x)
  |> printf "Day 2 Part 1: %d\n"
