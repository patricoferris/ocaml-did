(lang dune 3.17)

(name did)

(generate_opam_files true)

(source
 (github patricoferris/ocaml-did))

(authors "Patrick Ferris <patrick@sirref.org>")

(maintainers "Patrick Ferris <patrick@sirref.org>")

(license MIT)

(documentation https://ocaml.org/p/did)

(package
 (name did)
 (synopsis "OCaml DIDs")
 (description "A library for working with Decentralised Identifiers (DIDs) in OCaml")
 (depends
   ocaml
   multibase
   yojson
   uri
   (alcotest :with-test)
 )
 (tags
  ("add topics" "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
