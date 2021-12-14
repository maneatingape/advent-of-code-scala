package AdventOfCode2020

object Day13:
  def part1(input: Seq[String]): Long =
    val time = input(0).toInt
    val buses = input(1).split(",").filter(_ != "x").map(_.toInt).toSeq
    val (id, departs) = buses.map(id => id -> (id - time % id)).minBy(_._2)
    id * departs

  def part2(input: Seq[String]): Long =
    val buses = input(1).split(",").zipWithIndex.collect { case (rawId, offset) if rawId != "x" =>
      val id = rawId.toInt
      id -> ((id - offset % id) % id)
    }

    val (time, _) = buses.foldLeft((0L, 1L)) { case ((time, step), (id, remainder)) =>
      val nextTime = Iterator.iterate(time)(_ + step).dropWhile(_ % id != remainder).next()
      (nextTime, step * id)
    }

    time
  end part2

  def main(args: Array[String]): Unit =
    val data = io.Source.fromResource("AdventOfCode2020/Day13.txt").getLines().toSeq
    println(part1(data))
    println(part2(data))
