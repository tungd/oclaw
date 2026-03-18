(lang dune 3.15)

(package
 (name oclaw)
 (synopsis "OCaml 5 personal AI assistant with client-server architecture")
 (allow_empty)
 (depends
  (ocaml (>= 5.0))
  (dune (>= 3.15))
  domainslib
  sqlite3
  (h1 (>= 1.0.0))
  (iomux (>= 0.3.0))
  (ppx_protocol_conv (>= 5.2.3))
  (ppx_protocol_conv_json (>= 5.2.3))
  (ppx_protocol_conv_yaml (>= 5.2.3))
  yaml
  domainslib
  mosaic))
