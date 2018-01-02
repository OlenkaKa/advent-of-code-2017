package olenkaka.aoc2017.day

object DayFactory {
  def create(num: Int): Day = {
    require(num >= 1 && num <= 25)
    Day01
//    Class.forName("olenkaka.aoc2017.day.Day%02d".format(num)).newInstance.asInstanceOf[Day]
  }
}
