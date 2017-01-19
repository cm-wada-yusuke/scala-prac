package dwango.caseclass

sealed abstract class DayOfWeek

case object Sunday extends DayOfWeek

case object Monday extends DayOfWeek

case object Tuesday extends DayOfWeek

case object Wednesday extends DayOfWeek

case object Thursday extends DayOfWeek

case object Friday extends DayOfWeek

case object Saturday extends DayOfWeek

object DayOfWeekExecutor {

  def main(args: Array[String]): Unit = {
    println(nextDayOfWeek(Saturday))
  }

  def nextDayOfWeek(d: DayOfWeek): DayOfWeek = {
    d match {
      case Sunday => Monday
      case Monday => Tuesday
      case Tuesday => Wednesday
      case Wednesday => Thursday
      case Thursday => Friday
      case Friday => Saturday
      case Saturday => Sunday
    }
  }


}