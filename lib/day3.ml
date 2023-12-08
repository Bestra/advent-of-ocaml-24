open Core

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
    [%expect {|
        ((Number (43 0 0)) (Number (2 0 2))
        |}]
end
