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
    val root = (4 to 1 by - 1).foldLeft(number) { case (head +: tail, depth) =>
      tail.foldLeft(Seq(head)) { case (rest :+ last, next) =>
        if last.depth != depth || next.depth != depth then rest.appended(last).appended(next)
        else rest.appended(Node(3 * last.value + 2 * next.value, depth - 1))
      }
    }
    root.head.value

  def add(left: Number, right: Number): Number =
    val start = (left ++ right).map(node => Node(node.value, node.depth + 1))
    Iterator.iterate(Option(start))(_.flatMap(number => explode(number).orElse(split(number))))
      .takeWhile(_.isDefined)
      .toSeq.last.get

  def part1(input: Seq[String]): Int = magnitude(input.map(read).reduce(add))

  def part2(input: Seq[String]): Int =
    val numbers = input.map(read)
    val combinations = for first <- numbers; second <- numbers if first != second yield add(first, second)
    combinations.map(magnitude).max

  def main(args: Array[String]): Unit =
    val data = io.Source.fromResource("AdventOfCode2021/Day18.txt").getLines().toSeq
    println(part1(data))
    println(part2(data))
