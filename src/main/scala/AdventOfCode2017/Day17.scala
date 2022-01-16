package AdventOfCode2017

object Day17:
  case class Node(value: Int, var next: Node)

  def spin(count: Int, step: Int): (Int, Int) =
    val start = Node(0, null)
    start.next = start
    var current = start

    for n <- 1 to count do
      val insert = Iterator.iterate(current)(_.next).drop(step).next()
      current = Node(n, insert.next)
      insert.next = current

    (current.next.value, start.next.value)
  end spin

  def part1(input: Int): Int = spin(2017, input)._1

  def part2(input: Int): Int = spin(50000000, input)._2

  def main(args: Array[String]): Unit =
    val data = 304
    println(part1(data))
    println(part2(data))