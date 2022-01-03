package AdventOfCode2018

object Day04:
  def parse(input: Seq[String]): Map[Int, Map[Int, Int]] =
    val beginsShift = ".+#(\\d+) begins shift".r
    val fallsAsleep = ".+:(\\d+)] falls asleep".r
    val wakesUp = ".+:(\\d+)] wakes up".r
    val (asleep, _, _) = input.sorted.foldLeft((Map.empty[Int, Seq[Int]], -1, -1)) { case ((asleep, id, start), next) =>
      next match
        case beginsShift(nextId) => (asleep, nextId.toInt, start)
        case fallsAsleep(nextStart) => (asleep, id, nextStart.toInt)
        case wakesUp(end) => (asleep.updated(id, asleep.getOrElse(id, Seq()) ++ (start until end.toInt)), id, start)
    }
    asleep.map((id, minutes) => id -> minutes.groupMapReduce(identity)(_ => 1)(_ + _))

  def analyze(input: Seq[String], strategy: Iterable[Int] => Int): Int =
    val asleep = parse(input)
    val (id, minutes) = asleep.maxBy((id, minutes) => strategy(minutes.values))
    val (minute, _) = minutes.maxBy(_._2)
    id * minute

  def part1(input: Seq[String]): Int = analyze(input, _.sum)

  def part2(input: Seq[String]): Int = analyze(input, _.max)

  def main(args: Array[String]): Unit =
    val data = io.Source.fromResource("AdventOfCode2018/Day04.txt").getLines().toSeq
    println(part1(data))
    println(part2(data))
