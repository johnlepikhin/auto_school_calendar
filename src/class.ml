
type lesson = MyCalendar.Interval.t * string

type day = lesson list
					
module type T =
  sig
    val monday : day
    val thursday : day
    val wednesday : day
    val tuesday : day
    val friday : day
    val saturday : day
    val sunday : day
  end
