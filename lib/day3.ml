open Core

(* TODO: change number to use a list of coordinates*)
type entry = Symbol of (char * int * int) | Number of (int * int * int)
[@@deriving sexp_of]

module Parser = struct
  let is_digit = function '0' .. '9' -> true | _ -> false

  type t =
    { column_number: int
    ; int_buffer: string
    ; entries: entry list
    ; row_number: int }

  let flush_buffer t =
    if String.is_empty t.int_buffer then t
    else
      let number = int_of_string t.int_buffer in
      let entry = Number (number, t.row_number, t.column_number) in
      { t with
        int_buffer= ""
      ; entries= entry :: t.entries
      ; column_number= t.column_number + 1 }

  let append_buffer t c = {t with int_buffer= t.int_buffer ^ String.make 1 c}

  let symbol c t =
    let entry = Symbol (c, t.row_number, t.column_number) in
    {t with entries= entry :: t.entries}

  let advance t = {t with column_number= t.column_number + 1}

  let ingest_char c t =
    match c with
    | c when is_digit c ->
        append_buffer t c
    | '.' ->
        flush_buffer t |> advance
    | c ->
        flush_buffer t |> symbol c |> advance

  let read_row row row_number =
    let state = {column_number= 0; int_buffer= ""; entries= []; row_number} in
    let row = String.to_list row in
    let rec read_row' t l =
      match l with
      | [] ->
          let accum = flush_buffer t in
          List.rev accum.entries
      | c :: cs ->
          let new_state = ingest_char c t in
          read_row' new_state cs
    in
    read_row' state row

  let%expect_test _ =
    read_row "43.2.5" 0 |> [%sexp_of: entry list] |> print_s ;
    [%expect
      {|
        ((Number (43 0 0)) (Number (2 0 2)) (Number (5 0 4)))
        |}]

  let%expect_test _ =
    read_row "467..114.." 0 |> [%sexp_of: entry list] |> print_s ;
    [%expect {|
        ((Number (467 0 0)) (Number (114 0 3)))
        |}]

  let%expect_test _ =
    read_row ".....+.58." 0 |> [%sexp_of: entry list] |> print_s ;
    [%expect {|
        ((Symbol (+ 0 5)) (Number (58 0 7)))
        |}]

  let sample_input =
    [ "467..114.."
    ; "...*......"
    ; "..35..633."
    ; "......#..."
    ; "617*......"
    ; ".....+.58."
    ; "..592....."
    ; "......755."
    ; "...$.*...."
    ; ".664.598.." ]

  let parse_schematic schematic =
    let rec parse_schematic' schematic row_number =
      match schematic with
      | [] ->
          []
      | row :: rows ->
          let entries = read_row row row_number in
          entries @ parse_schematic' rows (row_number + 1)
    in
    parse_schematic' schematic 0

  let%expect_test _ =
    parse_schematic sample_input |> [%sexp_of: entry list] |> print_s ;
    [%expect
      {|
        ((Number (467 0 0)) (Number (114 0 3)) (Symbol (* 1 3)) (Number (35 2 2))
         (Number (633 2 5)) (Symbol (# 3 6)) (Number (617 4 0)) (Symbol (* 4 1))
         (Symbol (+ 5 5)) (Number (58 5 7)) (Number (592 6 2)) (Number (755 7 6))
         (Symbol ($ 8 3)) (Symbol (* 8 5)) (Number (664 9 1)) (Number (598 9 3)))
        |}]
end
