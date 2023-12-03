(lang dune 3.12)

(name advent)

(generate_opam_files true)

(source
 (github bestra/advent-of-ocaml-2024))

(authors "bestra")

(maintainers "bestra")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name advent)
 (synopsis "Solutions for Advent of Code in OCaml")
 (description "This project contains solutions for the Advent of Code challenges implemented in OCaml.")
 (depends ocaml dune core angstrom)
 (tags
  (topics "advent-of-code" "ocaml")))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
