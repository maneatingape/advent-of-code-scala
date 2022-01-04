package AdventOfCode2018

object Day08:
  case class Node(children: Seq[Node], metadata: Seq[Int])

  def parse(input: Seq[Int]): (Node, Seq[Int]) =
    val Seq(childCount, metadataCount, remaining1*) = input

    val (children, remaining2) = (0 until childCount).foldLeft((Seq.empty[Node], remaining1)) { case ((children, input), _) =>
      val (child, remaining) = parse(input)
      children.appended(child) -> remaining
    }

    val (metadata, remaining3) = remaining2.splitAt(metadataCount)
    Node(children, metadata) -> remaining3
  end parse

  def part1(input: Seq[Int]): Int =
    def helper(node: Node): Int = node.metadata.sum + node.children.map(helper).sum
    helper(parse(input)._1)

  def part2(input: Seq[Int]): Int =
    def helper(node: Node): Int = node match
      case Node(Seq(), metadata) => metadata.sum
      case Node(children, metadata) => metadata
        .filter(i => i > 0 && i <= children.size)
        .groupMapReduce(identity)(_ => 1)(_ + _)
        .map((k, v) => helper(children(k - 1)) * v)
        .sum

    helper(parse(input)._1)
  end part2

  def main(args: Array[String]): Unit =
    val data = io.Source.fromResource("AdventOfCode2018/Day08.txt").mkString.trim.split(" ").map(_.toInt).toSeq
    println(part1(data))
    println(part2(data))
