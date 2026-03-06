let setup_reporter ~style_renderer level =
  let dst = Fmt_tty.setup ~style_renderer stdout in
  let app = Fmt_tty.setup ~style_renderer stderr in
  Logs.set_level level;
  Logs.set_reporter (Logs_fmt.reporter ~app ~dst ())

let setup ?(level=Some Logs.Info) ?(format_time=true) () =
  ignore format_time;
  setup_reporter ~style_renderer:`Ansi_tty level

let supports_color () =
  match Sys.getenv_opt "NO_COLOR" with
  | Some _ -> false
  | None ->
      match Sys.getenv_opt "TERM" with
      | Some "dumb" | None -> false
      | Some _ ->
          try Unix.isatty (Unix.descr_of_out_channel stdout)
          with Unix.Unix_error _ -> false

let setup_auto ?(level=Some Logs.Info) ?(format_time=true) () =
  ignore format_time;
  let style_renderer = if supports_color () then `Ansi_tty else `None in
  setup_reporter ~style_renderer level
