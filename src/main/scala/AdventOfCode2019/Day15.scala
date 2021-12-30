package AdventOfCode2019

import Day09.IntCode

object Day15:
  case class Point(x: Int, y: Int):
    def +(other: Point): Point = Point(x + other.x, y + other.y)

  val neighbours = Seq(Point(0, -1) -> 1, Point(0, 1) -> 2, Point(-1, 0) -> 3, Point(1, 0) -> 4)

  def move(point: Point, computer: IntCode)(delta: Point, command: Int): (Point, IntCode, Long) =
    val nextPoint = point + delta
    val nextComputer = computer.withInput(command).nextOutput
    val IntCode.Output(status) = nextComputer.result
    (nextPoint, nextComputer, status)

  def dijkstra(initial: IntCode): (collection.mutable.Map[Point, Int], Option[(Point, IntCode)]) =
    val cost = collection.mutable.Map(Point(0, 0) -> 0)
    val todo = collection.mutable.PriorityQueue(Point(0, 0) -> initial)(Ordering.by((point, _) => cost(point)))
    var target = Option.empty[(Point, IntCode)]

    while todo.nonEmpty do
      val (point, computer) = todo.dequeue()
      neighbours
        .map(move(point, computer))
        .filter((next, _, _) => !cost.contains(next) || cost(point) + 1 < cost(next))
        .filter((_, _, status) => status > 0)
        .foreach { (next, computer, status) =>
          cost(next) = cost(point) + 1
          todo.enqueue(next -> computer)
          if status == 2 then target = Some(next -> computer)
        }
    end while

    (cost, target)
  end dijkstra

  def part1(memory: Seq[Long]): Int =
    val (first, Some(target, _)) = dijkstra(IntCode(memory))
    first(target)

  def part2(memory: Seq[Long]): Int =
    val (_, Some(_, computer)) = dijkstra(IntCode(memory))
    val (second, _) = dijkstra(computer)
    second.values.max

  def main(args: Array[String]): Unit =
    val data = io.Source.fromResource("AdventOfCode2019/Day15.txt").mkString.trim.split(",").map(_.toLong).toSeq
    println(part1(data))
    println(part2(data))
