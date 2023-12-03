open Core

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
  let combine_digits s = Int.of_string (String.of_char (first_digit s) ^ String.of_char (last_digit s))

  let%test _ = combine_digits "a1b2c3d4e5f" = 15
  let%test _ = combine_digits "treb7uchet" = 77

  (** sum the two digit numbers in the given list. for example with:
  1abc2
pqr3stu8vwx
a1b2c3d4e5f
treb7uchet    
the values are 12, 38, 15, 77 and the sum is 142
  *)

  let sum_two_digit_numbers l = List.fold l ~init:0 ~f:(fun acc s -> acc + combine_digits s)

  let%test _ = sum_two_digit_numbers ["1abc2"; "pqr3stu8vwx"; "a1b2c3d4e5f"; "treb7uchet"] = 142

  let part_1 () =
    let lines = read_file () in
    let sum = sum_two_digit_numbers lines in
    printf "sum of two digit numbers: %d\n" sum