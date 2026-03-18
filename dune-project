(lang dune 3.15)

(package
 (name oclaw)
 (synopsis "OCaml 5 CLI AI assistant with tool-first runtime")
 ;; CLI tool - no installation rules, run via dune exec
 (allow_empty)
 (depends
  (ocaml (>= 5.0))
  (dune (>= 3.15))
  domainslib
  sqlite3
  (h1 (>= 1.0.0))
  (iomux (>= 0.3.0))
  (jsonrpc (>= 0.21.0))
  (ppx_protocol_conv (>= 5.2.3))
  (ppx_protocol_conv_json (>= 5.2.3))
  (ppx_protocol_conv_yaml (>= 5.2.3))
  yaml
  domainslib
  mosaic))
