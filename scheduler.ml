module Log = (val Logs.src_log (Logs.Src.create "scheduler") : Logs.LOG)
module Config = Oclaw_config.Config

type handle = {
  stop_flag : bool Atomic.t;
  domain : unit Domain.t;
}

let truncate_summary text =
  let max_len = 300 in
  if String.length text <= max_len then text
  else String.sub text 0 max_len ^ "..."

let run_task state (task : Db.scheduled_task) =
  let started_at = Unix.gettimeofday () in
  let success, summary =
    match Agent_engine.process state ~chat_id:task.Db.chat_id task.Db.prompt with
    | Ok response -> (true, truncate_summary response)
    | Error err -> (false, truncate_summary ("Error: " ^ err))
  in
  let finished_at = Unix.gettimeofday () in
  let next_run_at, next_status =
    match task.Db.schedule_type with
    | "once" -> (None, "completed")
    | "cron" ->
        begin
          match Schedule_spec.next_cron_after task.Db.schedule_value ~after:finished_at with
          | Ok next_run -> (Some next_run, "active")
          | Error err ->
              Log.err (fun m -> m "Failed to compute next run for task %d: %s" task.Db.id err);
              (None, "error")
        end
    | other ->
        Log.err (fun m -> m "Unknown schedule_type for task %d: %s" task.Db.id other);
        (None, "error")
  in
  ignore
    (Db.insert_scheduled_task_run
       state.Runtime.db
       ~task_id:task.Db.id
       ~chat_id:task.Db.chat_id
       ~started_at
       ~finished_at
       ~success
       ~summary);
  ignore
    (Db.update_scheduled_task_after_run
       state.Runtime.db
       ~task_id:task.Db.id
       ~next_run_at
       ~status:next_status
       ~last_run_at:finished_at)

let run_due_tasks state =
  match Db.get_due_scheduled_tasks state.Runtime.db ~now:(Unix.gettimeofday ()) ~limit:8 with
  | Error err ->
      Log.err (fun m -> m "Failed to query due scheduled tasks: %s" err)
  | Ok tasks ->
      List.iter (run_task state) tasks

let rec sleep_until_next_poll stop_flag seconds =
  if seconds <= 0. || Atomic.get stop_flag then ()
  else
    let chunk = min 1.0 seconds in
    Unix.sleepf chunk;
    sleep_until_next_poll stop_flag (seconds -. chunk)

let worker_loop state stop_flag =
  while not (Atomic.get stop_flag) do
    run_due_tasks state;
    sleep_until_next_poll stop_flag (float_of_int Config.scheduler_poll_interval_seconds)
  done

let start state =
  if not Config.scheduler_enabled then None
  else
    let stop_flag = Atomic.make false in
    let domain = Domain.spawn (fun () -> worker_loop state stop_flag) in
    Some { stop_flag; domain }

let stop handle =
  Atomic.set handle.stop_flag true;
  Domain.join handle.domain
