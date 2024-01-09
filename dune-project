(lang dune 3.12)

(name opulse)

(generate_opam_files true)

(source
 (github mmartin/opulse))

(authors "Mārtiņš Mačs")

(maintainers "Mārtiņš Mačs")

(license MIT)

(package
 (name opulse)
 (synopsis "PulseAudio bindings for OCaml")
 (description "PulseAudio bindings for OCaml")
 (depends ocaml dune))

