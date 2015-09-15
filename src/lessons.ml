
open MyCalendar
open CalendarLib

let year_session = Interval.sdmake "2015-09-01" "2016-06-04"
    
let holidays = Interval.[
    sdmake "2015-10-05" "2015-10-12";
    sdmake "2015-11-16" "2015-11-23";
    sdmake "2015-12-30" "2016-01-07";
    sdmake "2016-02-15" "2016-02-22";
    sdmake "2016-04-04" "2016-04-11";
    of_sday "2015-11-04";
    of_sday "2016-01-07";
    of_sday "2016-02-23";
    of_sday "2016-03-08";
    of_sday "2016-05-02";
    of_sday "2016-05-09";
  ]

let is_holiday day =
  List.exists (fun interval -> Interval.within interval day) holidays
		       
let list (module C : Class.T) interval =
  let open Calendar in
  let rec loop today lst =
    if Interval.within interval today then
      let tomorrow = Period.lmake ~day:1 () |> add today in
      if is_holiday today then
	loop tomorrow lst
      else
	let wd = day_of_week today in
	let lessons =
	  match wd with
	  | Mon -> C.monday
	  | Tue -> C.thursday
	  | Wed -> C.wednesday
	  | Thu -> C.tuesday
	  | Fri -> C.friday
	  | Sat -> C.saturday
	  | Sun -> C.sunday
	in
	let lst =
	  List.fold_left (fun lst (i, l) ->
			  let i = Interval.set_date today i in
			  (i, l) :: lst
			 ) lst lessons
	in
	loop tomorrow lst
    else
      lst
  in
  loop interval.Interval._start []
