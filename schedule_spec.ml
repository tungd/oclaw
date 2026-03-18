type field =
  | Any
  | Values of int list

type cron = {
  minute : field;
  hour : field;
  day : field;
  month : field;
  weekday : field;
}

let trim = String.trim
let ( let* ) = Result.bind

let int_of_string_result value label =
  match int_of_string_opt (trim value) with
  | Some parsed -> Ok parsed
  | None -> Error ("invalid " ^ label ^ ": " ^ value)

let uniq_sorted values =
  values |> List.sort_uniq Int.compare

let parse_range ~min ~max label token =
  match String.split_on_char '-' token with
  | [ single ] ->
      let* value = int_of_string_result single label in
      if value < min || value > max then
        Error (Printf.sprintf "%s value %d is outside %d-%d" label value min max)
      else
        Ok [ value ]
  | [ left; right ] ->
      let* left = int_of_string_result left label in
      let* right = int_of_string_result right label in
      if left > right then
        Error (Printf.sprintf "%s range %d-%d is reversed" label left right)
      else if left < min || right > max then
        Error (Printf.sprintf "%s range %d-%d is outside %d-%d" label left right min max)
      else
        let rec loop acc value =
          if value < left then acc else loop (value :: acc) (value - 1)
        in
        Ok (loop [] right)
  | _ ->
      Error ("invalid " ^ label ^ " token: " ^ token)

let parse_field ~min ~max label raw =
  let raw = trim raw in
  if raw = "*" then Ok Any
  else
    raw
    |> String.split_on_char ','
    |> List.filter (fun token -> trim token <> "")
    |> List.fold_left
         (fun acc token ->
           let* acc = acc in
           let* values = parse_range ~min ~max label (trim token) in
           Ok (values @ acc))
         (Ok [])
    |> Result.map (fun values -> Values (uniq_sorted values))

let parse_cron cron =
  match cron |> trim |> String.split_on_char ' ' |> List.filter (fun part -> part <> "") with
  | [ minute; hour; day; month; weekday ] ->
      let* minute = parse_field ~min:0 ~max:59 "minute" minute in
      let* hour = parse_field ~min:0 ~max:23 "hour" hour in
      let* day = parse_field ~min:1 ~max:31 "day" day in
      let* month = parse_field ~min:1 ~max:12 "month" month in
      let* weekday = parse_field ~min:0 ~max:6 "weekday" weekday in
      Ok { minute; hour; day; month; weekday }
  | _ ->
      Error "cron must have exactly 5 fields: minute hour day month weekday"

let matches field value =
  match field with
  | Any -> true
  | Values values -> List.mem value values

let weekday_of_tm tm =
  match tm.Unix.tm_wday with
  | 0 -> 0
  | n -> n

let next_aligned_minute after =
  let rounded_down = floor (after /. 60.) *. 60. in
  rounded_down +. 60.

let next_cron_after cron ~after =
  let* cron = parse_cron cron in
  let rec loop candidate steps_remaining =
    if steps_remaining <= 0 then
      Error "could not find a matching cron occurrence within 2 years"
    else
      let tm = Unix.localtime candidate in
      if matches cron.minute tm.tm_min
         && matches cron.hour tm.tm_hour
         && matches cron.day tm.tm_mday
         && matches cron.month (tm.tm_mon + 1)
         && matches cron.weekday (weekday_of_tm tm)
      then
        Ok candidate
      else
        loop (candidate +. 60.) (steps_remaining - 1)
  in
  loop (next_aligned_minute after) (60 * 24 * 366 * 2)

let validate_cron cron =
  match parse_cron cron with
  | Ok _ -> Ok ()
  | Error _ as err -> err

let parse_date_time_parts value =
  let value = String.map (fun ch -> if ch = 'T' then ' ' else ch) (trim value) in
  match String.split_on_char ' ' value |> List.filter (fun part -> part <> "") with
  | [ date; time ] ->
      begin
        match String.split_on_char '-' date, String.split_on_char ':' time with
        | [ year; month; day ], [ hour; minute ] ->
            Ok (year, month, day, hour, minute, "00")
        | [ year; month; day ], [ hour; minute; second ] ->
            Ok (year, month, day, hour, minute, second)
        | _ ->
            Error "datetime must look like YYYY-MM-DDTHH:MM[:SS]"
      end
  | _ ->
      Error "datetime must look like YYYY-MM-DDTHH:MM[:SS]"

let parse_once value =
  let value = trim value in
  if value = "" then Error "run_at is required"
  else
    match float_of_string_opt value with
    | Some timestamp -> Ok timestamp
    | None ->
        let* year, month, day, hour, minute, second = parse_date_time_parts value in
        let* year = int_of_string_result year "year" in
        let* month = int_of_string_result month "month" in
        let* day = int_of_string_result day "day" in
        let* hour = int_of_string_result hour "hour" in
        let* minute = int_of_string_result minute "minute" in
        let* second = int_of_string_result second "second" in
        let tm =
          {
            Unix.tm_sec = second;
            tm_min = minute;
            tm_hour = hour;
            tm_mday = day;
            tm_mon = month - 1;
            tm_year = year - 1900;
            tm_wday = 0;
            tm_yday = 0;
            tm_isdst = false;
          }
        in
        let timestamp, _ = Unix.mktime tm in
        Ok timestamp

let format_timestamp_local timestamp =
  let tm = Unix.localtime timestamp in
  Printf.sprintf "%04d-%02d-%02d %02d:%02d:%02d"
    (tm.tm_year + 1900)
    (tm.tm_mon + 1)
    tm.tm_mday
    tm.tm_hour
    tm.tm_min
    tm.tm_sec
