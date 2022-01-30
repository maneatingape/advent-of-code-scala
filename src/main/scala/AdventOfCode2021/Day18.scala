package AdventOfCode2021

object Day18:
  case class Node(value: Int, depth: Int)
  type Number = Seq[Node]

  def read(input: String): Number =
    val (_, number) = input.foldLeft(0 -> Seq.empty[Node]) { case ((depth, number), next) =>
      next match
        case '[' => (depth + 1) -> number
        case ']' => (depth - 1) -> number
        case ',' => depth -> number
        case _ => depth -> number.appended(Node(next.asDigit, depth))
    }
    number

  def explode(number: Number): Option[Number] = number.find(_.depth == 5).map { node =>
    val (left, right) = number.splitAt(number.indexOf(node) + 1)

    val nextLeft = left match
      case rest :+ previous :+ left => rest.appended(Node(previous.value + left.value, previous.depth))
      case _ => Seq()

    val nextRight = right match
      case right +: next +: rest => rest.prepended(Node(next.value + right.value, next.depth))
      case _ => Seq()

    nextLeft ++ Seq(Node(0, 4)) ++ nextRight
  }

  def split(number: Number): Option[Number] = number.find(_.value >= 10).map { node =>
    val slice = Seq(Node(node.value / 2, node.depth + 1), Node(node.value / 2 + node.value % 2, node.depth + 1))
    number.patch(number.indexOf(node), slice, 1)
  }

  def magnitude(number: Number): Int =
    def helper(number: Number): Number = number match
      case Seq(left, right, rest*) if left.depth == right.depth =>
        val merged = Node(3 * left.value + 2 * right.value, left.depth - 1)
        helper(helper(rest).prepended(merged))
      case other => other
    helper(number).head.value

  def add(left: Number, right: Number): Number =
    def helper(number: Number): Number = explode(number).orElse(split(number)) match
      case Some(next) => helper(next)
      case None => number
    helper((left ++ right).map(node => Node(node.value, node.depth + 1)))

  def part1(input: Seq[String]): Int = magnitude(input.map(read).reduce(add))

  def part2(input: Seq[String]): Int =
    val numbers = input.map(read)
    val combinations = for first <- numbers; second <- numbers if first != second yield add(first, second)
    combinations.map(magnitude).max

  def main(args: Array[String]): Unit =
    val data = io.Source.fromResource("AdventOfCode2021/Day18.txt").getLines().toSeq
    println(part1(data))
    println(part2(data))
