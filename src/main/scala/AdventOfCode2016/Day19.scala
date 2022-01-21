package AdventOfCode2016

object Day19:
  def part1(input: Int): Int = ((input - Integer.highestOneBit(input)) << 1) + 1

  def part2(input: Int): Int =
    case class Node(value: Int, var next: Node)

    var node = Node(1, null)
    node.next = node

    for n <- 2 to input do
      val next = Node(n, node.next)
      node.next = next
      node = next

    for i <- 1 to input / 2 do
      node = node.next

    for i <- input to 2 by -1 do
      node.next = node.next.next
      if i % 2 == 1 then node = node.next

    node.value
  end part2

  def main(args: Array[String]): Unit =
    val data = io.Source.fromResource("AdventOfCode2016/Day19.txt").mkString.trim.toInt
    println(part1(data))
    println(part2(data))
