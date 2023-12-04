open Core
open Angstrom

let read_file () = In_channel.read_lines "./inputs/1.txt"

let rec fact n = if n = 1 then 1 else n * fact (n - 1)

let%test _ = fact 5 = 120

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
    [ string "one" *> return (Some 1)
    ; string "1" *> return (Some 1)
    ; string "two" *> return (Some 2)
    ; string "2" *> return (Some 2)
    ; string "three" *> return (Some 3)
    ; string "3" *> return (Some 3)
    ; string "four" *> return (Some 4)
    ; string "4" *> return (Some 4)
    ; string "five" *> return (Some 5)
    ; string "5" *> return (Some 5)
    ; string "six" *> return (Some 6)
    ; string "6" *> return (Some 6)
    ; string "seven" *> return (Some 7)
    ; string "7" *> return (Some 7)
    ; string "eight" *> return (Some 8)
    ; string "8" *> return (Some 8)
    ; string "nine" *> return (Some 9)
    ; string "9" *> return (Some 9) ]

let my_parser' = one_through_nine <|> advance 1 *> return None

let my_parser = many1 my_parser'

let filter_some l = List.filter_opt l

let parse_one_through_nine s =
  parse_string my_parser s ~consume:All |> Result.map ~f:filter_some

(* filter out the list and only return Some results omitting None*)

let print_result r =
  Result.sexp_of_t [%sexp_of: int list] [%sexp_of: string] r
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
