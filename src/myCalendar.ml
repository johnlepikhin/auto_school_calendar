
open CalendarLib

let () =
  Time_Zone.change (Time_Zone.Local)
       
module Interval =
  struct
    type t = {
	_start : Calendar.t;
	_end : Calendar.t;
      }

    open Calendar

    let to_string t =
      let open Printer.Calendar in
      Printf.sprintf "%s -> %s" (to_string t._start) (to_string t._end)
	   
    let make _start _end =
      let add_second s = Calendar.Period.lmake ~second:1 () |> Calendar.add s in
      let t = { _start = add_second _start; _end = add_second _end } in
      if compare _start _end >= 0 then
	Util.fatal_exit (to_string t |> Printf.sprintf "Некорректный интервал: %s");
      t

    let smake sstart send =
      let open Printer.Calendar in
      make (from_string sstart) (from_string send)

    let sdmake sstart send =
      let of_string s =
	Printer.Calendar.from_string (s ^ " 00:00:00")
      in
      make (of_string sstart) (of_string send)

    let default_date = Date.make 0 0 0
	   
    let stmake sstart send =
      let create s =
	Printer.Time.from_string (s ^ ":00") |> create default_date
      in
      make (create sstart) (create send)

    let of_sday date =
      let open Printer.Calendar in
      let _start = from_string (date ^ " 00:00:00") in
      let _end = Calendar.Period.lmake ~day:1 () |> Calendar.add _start in
      make _start _end
	   
    let within t time =
      compare t._start time <= 0 && compare t._end time > 0

    let set_date d t =
      let update date =
	to_time date |> create (to_date d)
      in
      {
	_start = update t._start;
	_end = update t._end;
      }
  end

let is_weekend t =
  let open Calendar in
  match day_of_week t with
  | Sat | Sun -> true
  | _ -> false
