(lang dune 3.7)
(using menhir 2.1)
(generate_opam_files true)

(package
 (name ollisp)
 (depends menhir))
