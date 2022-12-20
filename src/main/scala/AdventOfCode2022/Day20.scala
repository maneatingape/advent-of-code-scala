package AdventOfCode2022

object Day20:
  case class Node(value: Long, var prev: Node, var next: Node)

  def parse(input: Seq[Int], key: Long): Seq[Node] =
    val nodes = input.map(n => Node(n * key, null, null))
    nodes.zipWithIndex.foreach { (node, i) =>
      node.prev = nodes(if i > 0 then i - 1 else input.size - 1)
      node.next = nodes(if i + 1 < input.size then i + 1 else 0)
    }
    nodes

  def mix(nodes: Seq[Node]): Unit =
    for node <- nodes do
      val move = (node.value % (nodes.size - 1)).toInt
      for _ <- 1 to move.abs do
        val (a, b, c, d) =
          if (move > 0) (node.prev, node, node.next, node.next.next)
          else (node.prev.prev, node.prev, node, node.next)
        a.next = c
        b.prev = c
        b.next = d
        c.prev = a
        c.next = b
        d.prev = b
      end for
    end for

  def decrypt(input: Seq[Int], key: Long, rounds: Int): Long =
    val nodes = parse(input, key)
    for _ <- 1 to rounds do mix(nodes)
    val zero = nodes.find(_.value == 0).get
    def next(start: Node): Node = Iterator.iterate(start)(_.next).drop(1000).next()
    Iterator.iterate(zero)(next).drop(1).take(3).map(_.value).sum

  def part1(input: Seq[Int]): Long = decrypt(input, 1, 1)

  def part2(input: Seq[Int]): Long = decrypt(input, 811589153L, 10)

  def main(args: Array[String]): Unit =
    val data = io.Source.fromResource("AdventOfCode2022/Day20.txt").getLines().map(_.toInt).toSeq
    println(part1(data))
    println(part2(data))
