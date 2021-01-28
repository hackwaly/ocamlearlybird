(lang dune 2.8)
(using menhir 2.1)

(name earlybird-411)

(generate_opam_files true)
(authors hackwaly@qq.com)
(maintainers hackwaly@qq.com)
(source (github hackwaly/ocamlearlybird))

(homepage https://github.com/hackwaly/ocamlearlybird)
(bug_reports https://github.com/hackwaly/ocamlearlybird/issues)

(package
  (name earlybird-411)
  (synopsis "Debug adapter for OCaml 4.11")
  (description "Debug adapter for OCaml 4.11.")
  (depends
    (ocaml (and (>= 4.11.0) (< 4.12.0)))
    (ppx_deriving (>= 5.1))
    (ppx_deriving_yojson (>= 3.6.1))
    (menhir (and (>= 20201216) :build))
    (menhirLib (>= 20201216))
    (iter (>= 1.2.1))
    (lwt (>= 5.4.0))
    (lwt_ppx (>= 2.0.1))
    (lwt_react (>= 1.1.3))
    (cmdliner (>= 1.0.4))
    (logs (>= 0.7.0))
    (fmt (>= 0.8.9))
    (dap (>= 1.0.5))))
