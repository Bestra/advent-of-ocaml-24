open Core
open Angstrom

let read_file () = In_channel.read_lines "./inputs/1.txt"

(** Find the first digit character in the given string*)
let first_digit s =
  let rec find_digit' i =
    if Char.is_digit s.[i] then s.[i] else find_digit' (i + 1)
  in
  find_digit' 0

(** find the last digit character in the give string*)
let last_digit s =
  let rec find_digit' i =
    if Char.is_digit s.[i] then s.[i] else find_digit' (i - 1)
  in
  find_digit' (String.length s - 1)

let%test _ = Char.(first_digit "a1b2c3d4e5f" = '1')

let%test _ = Char.(last_digit "a1b2c3d4e5f" = '5')

(** combine the first and last digits into a two digit number*)
let combine_digits s =
  Int.of_string (String.of_char (first_digit s) ^ String.of_char (last_digit s))

let%test _ = combine_digits "a1b2c3d4e5f" = 15

let%test _ = combine_digits "treb7uchet" = 77

(** sum the two digit numbers in the given list. for example with:
  1abc2
pqr3stu8vwx
a1b2c3d4e5f
treb7uchet    
the values are 12, 38, 15, 77 and the sum is 142
  *)

let sum_two_digit_numbers l =
  List.fold l ~init:0 ~f:(fun acc s -> acc + combine_digits s)

let%test _ =
  sum_two_digit_numbers ["1abc2"; "pqr3stu8vwx"; "a1b2c3d4e5f"; "treb7uchet"]
  = 142

let part_1 () =
  let lines = read_file () in
  let sum = sum_two_digit_numbers lines in
  printf "sum of two digit numbers: %d\n" sum

let one_through_nine =
  choice
    [ string "zero" *> return (Some '0')
    ; string "0" *> return (Some '0')
    ; string "one" *> return (Some '1')
    ; string "1" *> return (Some '1')
    ; string "two" *> return (Some '2')
    ; string "2" *> return (Some '2')
    ; string "three" *> return (Some '3')
    ; string "3" *> return (Some '3')
    ; string "four" *> return (Some '4')
    ; string "4" *> return (Some '4')
    ; string "five" *> return (Some '5')
    ; string "5" *> return (Some '5')
    ; string "six" *> return (Some '6')
    ; string "6" *> return (Some '6')
    ; string "seven" *> return (Some '7')
    ; string "7" *> return (Some '7')
    ; string "eight" *> return (Some '8')
    ; string "8" *> return (Some '8')
    ; string "nine" *> return (Some '9')
    ; string "9" *> return (Some '9') ]

let my_parser' = one_through_nine <|> advance 1 *> return None

let my_parser = many1 my_parser'

let parse_one_through_nine s =
  let filter_some l = List.filter_opt l in
  parse_string my_parser s ~consume:All |> Result.map ~f:filter_some

let print_result r =
  Result.sexp_of_t [%sexp_of: char list] [%sexp_of: string] r
  |> Sexp.to_string_hum |> print_endline

let%expect_test _ =
  parse_one_through_nine "one" |> print_result ;
  [%expect {|
    (Ok (1))
  |}]

let%expect_test _ =
  parse_one_through_nine "heythreedude4" |> print_result ;
  [%expect {|
    (Ok (3 4))
  |}]

let first_and_last l =
  match l with
  | [] ->
      failwith "Empty list"
  | [x] ->
      (x, x)
  | first :: rest ->
      (first, List.last_exn rest)

let%expect_test _ =
  first_and_last [1; 2; 3; 4; 5]
  |> [%sexp_of: int * int] |> Sexp.to_string_hum |> print_endline ;
  [%expect {| (1 5) |}]

let%expect_test _ =
  first_and_last [1] |> [%sexp_of: int * int] |> Sexp.to_string_hum
  |> print_endline ;
  [%expect {| (1 1) |}]

let first_and_last_digits s =
  parse_one_through_nine s
  |> Result.map ~f:first_and_last
  |> Result.ok_or_failwith

(**
    
two1nine
eightwothree
abcone2threexyz
xtwone3four
4nineeightseven2
zoneight234
7pqrstsixteen
In this example, the calibration values are 29, 83, 13, 24, 42, 14, and 76. Adding these together produces 281.
Write tests for each of these
*)

let print_result' r =
  [%sexp_of: char * char] r |> Sexp.to_string_hum |> print_endline

let%expect_test _ =
  first_and_last_digits "two1nine" |> print_result' ;
  [%expect {| (2 9) |}]

let%expect_test _ =
  first_and_last_digits "eightwothree" |> print_result' ;
  [%expect {| (8 3) |}]

let%expect_test _ =
  first_and_last_digits "two65eightbkgqcsn91qxkfvg" |> print_result' ;
  [%expect {| (2 1) |}]

let calibration s =
  let a, b = first_and_last_digits s in
  Int.of_string (String.of_char a ^ String.of_char b)

let calibration_sum lines =
  List.fold lines ~init:0 ~f:(fun acc s ->
      let c = calibration s in
      printf "%s -> %d\n" s c ;
      acc + calibration s )

let%expect_test _ =
  calibration "two65eightbkgqcsn91qxkfvg" |> printf "%d\n" ;
  [%expect {| 21 |}]

let%expect_test _ =
  calibration_sum
    [ "two1nine"
    ; "eightwothree"
    ; "abcone2threexyz"
    ; "xtwone3four"
    ; "4nineeightseven2"
    ; "zoneight234"
    ; "7pqrstsixteen" ]
  |> printf "%d\n" ;
  [%expect {| 281 |}]

let part_2 () =
  let lines = read_file () in
  printf "sum of two digit numbers: %d\n" (calibration_sum lines)
