open Core

type entry = Symbol of (char * int * int) | Number of (int * int * int)
[@@deriving sexp_of]

module Parser = struct
  let is_digit = function '0' .. '9' -> true | _ -> false

  type t =
    { row_number: int
    ; column_number: int
    ; entries: entry list
    ; int_buffer: string }

  let read_row row row_number =
    let rec read_row' acc column int_buffer = function
      | [] ->
          acc
      | c :: cs ->
          if is_digit c then
            read_row' acc (column + 1) (int_buffer ^ String.make 1 c) cs
          else if String.is_empty int_buffer then
            if Char.(c = '.') then read_row' acc (column + 1) "" cs
            else
              read_row'
                (Symbol (c, row_number, column) :: acc)
                (column + 1) "" cs
          else
            let number = int_of_string int_buffer in
            read_row'
              (Number (number, row_number, column) :: acc)
              (column + 1) "" cs
    in
    read_row' [] 0 "" row
end
