package AdventOfCode2017

object Day07:
  case class Node(name: String, weight: Int, total: Int, unbalanced: Boolean, children: Seq[Node])

  def parse(input: Seq[String]): Node =
    val regex = """(\w+) \((\d+)\).*""".r
    val weights = input.map { case regex(name, weight) => name -> weight.toInt }.toMap
    val edges = input.map { line =>
      val Array(name, _, children*) = line.split("[^\\d\\w]+")
      name -> children
    }
    .toMap

    def helper(name: String): Node =
      val children = edges(name).map(helper)
      val total = weights(name) + children.map(_.total).sum
      val unbalanced = children.exists(_.total != children.head.total)
      Node(name, weights(name), total, unbalanced, children)

    val nonRoot = edges.values.flatten.toSet
    val root = edges.keys.filterNot(nonRoot.contains).head
    helper(root)
  end parse

  def part1(input: Seq[String]): String = parse(input).name

  def part2(input: Seq[String]): Int =
    def helper(node: Node): Int = node.children.find(_.unbalanced) match
      case Some(unbalanced) => helper(unbalanced)
      case None => node.children.partition(_.total == node.children.head.total) match
        case (Seq(other), Seq(same, _*)) => other.weight + same.total - other.total
        case (Seq(same, _*), Seq(other)) => other.weight + same.total - other.total

    helper(parse(input))
  end part2

  def main(args: Array[String]): Unit =
    val data = io.Source.fromResource("AdventOfCode2017/Day07.txt").getLines().toSeq
    println(part1(data))
    println(part2(data))
