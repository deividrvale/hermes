(lang dune 3.9)

(using menhir 2.1)

(name hermes)

(version 1.0.0)

(generate_opam_files true)

(source
 (github deividrvale/hermes))

(authors "Deivid Vale" "Liye Guo")

(maintainers "Deivid Vale" "Liye Guo")

(license MIT)

(documentation https://url/to/documentation)

(package
 (name hermes)
 (synopsis "A first-order rewriting termination and complexity analyzer.")
 (description "Hermes is a first-order rewriting termination and complexity analyzer\n
 for term rewriting systems with innermost rewriting strategy.")
 (depends
  (ocaml  (>= 4.14.0))
  (dune   (>= 3.9.1))
  (menhir (>= 20230608))
  (z3     (>= 4.12.2-1)))
 (tags
  (topics
   "first-order rewriting"
   termination
   complexity
   innermost
   "automated reasoning")))
