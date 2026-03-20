(lang dune 3.15)

(package
 (name oclaw)
 (synopsis "OCaml 5 CLI AI assistant with tool-first runtime")
 (authors "OClaw Contributors")
 (maintainers "OClaw Maintainers")
 (license MIT)
 (depends
  (ocaml (>= 5.0))
  (dune (>= 3.15))
  crunch
  domainslib
  sqlite3
  (h1 (>= 1.0.0))
  (iomux (>= 0.3.0))
  (jsonrpc (>= 0.21.0))
  ppx_deriving_yojson
  ppx_deriving_yaml
  yaml
  mosaic
  (tyxml (>= 4.6.0))
  curl
  yojson
  logs
  str))
