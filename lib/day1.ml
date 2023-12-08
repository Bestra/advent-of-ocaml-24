open Core
open Base.Poly

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

let sum_two_digit_numbers l =
  List.fold l ~init:0 ~f:(fun acc s -> acc + combine_digits s)

let%test _ =
  sum_two_digit_numbers ["1abc2"; "pqr3stu8vwx"; "a1b2c3d4e5f"; "treb7uchet"]
  = 142

let part_1 () =
  let lines = read_file () in
  let sum = sum_two_digit_numbers lines in
  printf "sum of two digit numbers: %d\n" sum

(** sum the two digit numbers in the given list. for example with:
  1abc2
pqr3stu8vwx
a1b2c3d4e5f
treb7uchet    
the values are 12, 38, 15, 77 and the sum is 142
  *)

(* Given a list, match the first characters for those contained in the words
   one - nine, one, two, three, etc, as well as 1-9 characters.
   if patterns end with e, o, or t, we don't want to consume that final character. that'll let us match things like oneight and spit out [1; 8] *)
let extract_digits_from_string s =
  let rec extract_digits acc l =
    match l with
    | [] ->
        List.rev acc
    | ('1' .. '9' as c) :: rest ->
        extract_digits (c :: acc) rest
    | 'o' :: 'n' :: 'e' :: tl ->
        extract_digits ('1' :: acc) ('e' :: tl)
    | 't' :: 'w' :: 'o' :: tl ->
        extract_digits ('2' :: acc) ('o' :: tl)
    | 't' :: 'h' :: 'r' :: 'e' :: 'e' :: tl ->
        extract_digits ('3' :: acc) ('e' :: tl)
    | 'f' :: 'o' :: 'u' :: 'r' :: tl ->
        extract_digits ('4' :: acc) tl
    | 'f' :: 'i' :: 'v' :: 'e' :: tl ->
        extract_digits ('5' :: acc) ('e' :: tl)
    | 's' :: 'i' :: 'x' :: tl ->
        extract_digits ('6' :: acc) tl
    | 's' :: 'e' :: 'v' :: 'e' :: 'n' :: tl ->
        extract_digits ('7' :: acc) tl
    | 'e' :: 'i' :: 'g' :: 'h' :: 't' :: tl ->
        extract_digits ('8' :: acc) ('t' :: tl)
    | 'n' :: 'i' :: 'n' :: 'e' :: tl ->
        extract_digits ('9' :: acc) ('e' :: tl)
    | _ :: rest ->
        extract_digits acc rest
  in
  extract_digits [] (String.to_list s)

let%expect_test _ =
  extract_digits_from_string "1abc2"
  |> [%sexp_of: char list] |> Sexp.to_string_hum |> print_endline ;
  [%expect {| (1 2) |}]

let%expect_test _ =
  extract_digits_from_string "oneightwone"
  |> [%sexp_of: char list] |> Sexp.to_string_hum |> print_endline ;
  [%expect {| (1 8 2 1) |}]

let%test _ = extract_digits_from_string "pqr3stu8vwx" = ['3'; '8']

let%test _ = extract_digits_from_string "one" = ['1']

let first_and_last_digits s =
  let digit_list = extract_digits_from_string s in
  (List.hd_exn digit_list, List.last_exn digit_list)

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
  first_and_last_digits "eightwo" |> print_result' ;
  [%expect {| (8 2) |}]

let%expect_test _ =
  first_and_last_digits "two65eightbkgqcsn91qxkfvg" |> print_result' ;
  [%expect {| (2 1) |}]

let calibration s =
  let a, b = first_and_last_digits s in
  Int.of_string (String.of_char a ^ String.of_char b)

let calibration_sum lines =
  List.fold lines ~init:0 ~f:(fun acc s -> acc + calibration s)

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
