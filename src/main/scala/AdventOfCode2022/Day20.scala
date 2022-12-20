package AdventOfCode2022

object Day20:
  case class Node(value: Long, var prev: Node, var next: Node)

  def parse(input: Seq[Int], key: Long): Seq[Node] =
    val nodes = input.map(n => Node(n * key, null, null))
    nodes.zipWithIndex.foreach { (node, i) =>
      node.prev = nodes((i - 1 + nodes.size) % nodes.size)
      node.next = nodes((i + 1) % nodes.size)
    }
    nodes

  def mix(nodes: Seq[Node]): Unit =
    for node <- nodes do
      val remainder = (node.value % (nodes.size - 1)).toInt
      val move = if remainder >= 0 then remainder else remainder + nodes.size - 1
      for _ <- 1 to move do
        val (a, b, c, d) = (node.prev, node, node.next, node.next.next)
        a.next = c
        b.prev = c
        b.next = d
        c.prev = a
        c.next = b
        d.prev = b
      end for
    end for

  def skip(start: Node): Node = Iterator.iterate(start)(_.next).drop(1000).next()

  def decrypt(input: Seq[Int], key: Long, rounds: Int): Long =
    val nodes = parse(input, key)
    for _ <- 1 to rounds do mix(nodes)
    val start = nodes.find(_.value == 0).get
    Iterator.iterate(start)(skip).drop(1).take(3).map(_.value).sum

  def part1(input: Seq[Int]): Long = decrypt(input, 1, 1)

  def part2(input: Seq[Int]): Long = decrypt(input, 811589153L, 10)

  def main(args: Array[String]): Unit =
    val data = io.Source.fromResource("AdventOfCode2022/Day20.txt").getLines().map(_.toInt).toSeq
    println(part1(data))
    println(part2(data))
