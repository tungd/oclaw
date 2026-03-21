let html (app : App.t) ~chat_id ~out_path =
  Transcript.export_html (App.internal_state app).Runtime.transcript ~chat_id ~out_path
