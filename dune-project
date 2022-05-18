(lang dune 3.0)

(using menhir 2.1)

(name hermes)

(version 0.1.0)

(generate_opam_files true)

(source
 (github deividrvale/hermes))

(authors "Deivid Vale" "Liye Guo")

(maintainers "Deivid Vale" "Liye Guo")

(license MIT)

(documentation https://url/to/documentation)

(package
 (name hermes)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends ocaml dune)
 (tags
  (topics "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
