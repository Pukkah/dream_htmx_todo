(lang dune 3.10)

(name hello_dream)

(generate_opam_files true)

(source
 (github Pukkah/hello-dream))

(authors "Miks Ramāns")

(maintainers "Maintainer Name")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name hello_dream)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends ocaml dune)
 (tags
  (topics "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
