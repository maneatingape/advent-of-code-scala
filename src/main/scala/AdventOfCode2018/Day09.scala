package AdventOfCode2018

object Day09:
  case class Node(value: Int, var previous: Node, var next: Node)

  def root: Node =
    val node = Node(0, null, null)
    node.previous = node
    node.next = node
    node

  def play(input: String, multiplier: Int): Long =
    val Array(players, last) = input.split("\\D+").map(_.toInt)
    val scores = Array.fill(players)(0L)
    var current = root

    for marble <- 1 to last * multiplier do
      if marble % 23 > 0 then
        val left = current.next
        val right = current.next.next
        current = Node(marble, left, right)
        left.next = current
        right.previous = current
      else
        val node = current.previous.previous.previous.previous.previous.previous.previous
        node.previous.next = node.next
        node.next.previous = node.previous
        current = node.next
        scores(marble % players) += marble + node.value
    end for

    scores.max
  end play

  def part1(input: String): Long = play(input, 1)

  def part2(input: String): Long = play(input, 100)

  def main(args: Array[String]): Unit =
    val data = io.Source.fromResource("AdventOfCode2018/Day09.txt").mkString.trim
    println(part1(data))
    println(part2(data))
