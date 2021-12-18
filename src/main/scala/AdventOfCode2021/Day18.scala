package AdventOfCode2021

object Day18:
  sealed trait Node
  case class Pair(left: Node, right: Node) extends Node
  case class Leaf(value: Int) extends Node

  type Path = List[Node]
  extension (path: Path)
    def exactlyEqual(other: Path) = path.length == other.length && path.zip(other).forall(_ eq _) // Careful - use strict reference equality when comparing paths

  def read(input: String): Node =
    def parse(string: String): (String, Node) = string(1) match
      case '[' => helper(string.drop(1))
      case digit => (string.drop(2), Leaf(digit.asDigit))

    def helper(remaining: String): (String, Node) =
      val (first, left) = parse(remaining)
      val (second, right) = parse(first)
      (second.drop(1), Pair(left, right))
    end helper

    val (_, root) = helper(input)
    root
  end read

  def write(node: Node): String = node match
    case Leaf(value) => value.toString
    case Pair(left, right) => "[" + write(left) + "," + write(right) + "]"

  def magnitude(node: Node ): Int = node match
    case Leaf(value) => value
    case Pair(left, right) => 3 * magnitude(left) + 2 * magnitude(right)

  def findSplit(root: Node): Option[Path] =
    def helper(node: Node, path: Path): Option[Path] = node match
      case Leaf(value) => Option.when(value >= 10)(node :: path)
      case Pair(left, right) => helper(left, node :: path).orElse(helper(right, node :: path))

    helper(root, Nil)
  end findSplit

  def handleSplit(root: Node)(splitter: Path): Node =
    def helper(node: Node, path: Path): Node = node match
      case Leaf(value) if splitter.exactlyEqual(node :: path) => Pair(Leaf(value / 2), Leaf(value / 2 + value % 2))
      case Pair(left, right) => Pair(helper(left, node :: path), helper(right, node :: path))
      case _ => node

    helper(root , Nil)
  end handleSplit

  def findExplode(root: Node): Option[Path] =
    def helper(node: Node, depth: Int, path: List[Node]): Option[List[Node]] = node match
      case Leaf(_) => None
      case Pair(Leaf(_), Leaf(_)) if depth == 4 => Some(node :: path)
      case Pair(left, right) => helper(left, depth + 1, node :: path).orElse(helper(right, depth + 1, node :: path))

    helper(root, 0 , Nil)
  end findExplode

  def handleExplode(root: Node)(exploder: Path): Node =
    val (left, right) = exploder.head match
      case Pair(Leaf(left), Leaf(right)) => (left, right)

    def upLeft(path: Path): Option[Path] = path match
      case child :: Pair(left, _) :: tail if left.ne(child) => Some(downRight(left, path.tail)) // Careful - strict reference equality
      case child :: Nil => None
      case child :: tail => upLeft(tail)

    def downRight(node: Node, path: Path): Path = node match
      case Leaf(_) => node :: path
      case Pair(_, right) => downRight(right, node :: path)

    def upRight(path: Path): Option[Path] = path match
      case child :: Pair(_, right) :: tail if right.ne(child) => Some(downLeft(right, path.tail)) // Careful - strict reference equality
      case child :: Nil => None
      case child :: tail => upRight(tail)

    def downLeft(node: Node, path: Path): Path = node match
      case Leaf(_) => node :: path
      case Pair(left, _) => downLeft(left, node :: path)

    val firstLeft = upLeft(exploder)
    val firstRight = upRight(exploder)

    def helper(node: Node, path: Path): Node = node match
      case Leaf(value) if firstLeft.exists(_.exactlyEqual(node :: path)) => Leaf(value + left)
      case Leaf(value) if firstRight.exists(_.exactlyEqual(node :: path)) => Leaf(value + right)
      case Pair(_, _) if exploder.exactlyEqual(node :: path) => Leaf(0)
      case Pair(left, right) => Pair(helper(left, node :: path), helper(right, node :: path))
      case _ => node

    helper(root, Nil)
  end handleExplode

  def add(left: Node, right: Node): Node =
    Iterator.iterate(Option[Node](Pair(left, right))) { case Some(state) =>
      findExplode(state).map(handleExplode(state)).orElse(findSplit(state).map(handleSplit(state)))
    }
    .takeWhile(_.isDefined).toSeq.last.get

  def part1(input: Seq[String]): Int = magnitude(input.map(read).reduce(add))

  def part2(input: Seq[String]): Int = input.map(read)
    .toSet.subsets(2)
    .flatMap { set =>
      Seq((set.head, set.last), (set.last, set.head))
    }
    .map(add).map(magnitude).max

  def main(args: Array[String]): Unit =
    val data = io.Source.fromResource("AdventOfCode2021/Day18.txt").getLines().toSeq
    println(part1(data))
    println(part2(data))
