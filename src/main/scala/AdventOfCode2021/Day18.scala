package AdventOfCode2021

object Day18:
  sealed trait Node
  case class Leaf(value: Int) extends Node
  case class Pair(left: Node, right: Node) extends Node

  def read(input: String): Node =
    def parseNode(string: String): (Node, String) = string.head match
      case '[' => parsePair(string.tail)
      case digit => (Leaf(digit.asDigit), string.tail)
    def parsePair(string: String): (Node, String) =
      val (left, first) = parseNode(string)
      val (right, second) = parseNode(first.tail)
      (Pair(left, right), second.tail)

    parseNode(input)._1
  end read

  def write(node: Node): String = node match
    case Leaf(value) => value.toString
    case Pair(left, right) => "[" + write(left) + "," + write(right) + "]"

  def magnitude(node: Node ): Int = node match
    case Leaf(value) => value
    case Pair(left, right) => 3 * magnitude(left) + 2 * magnitude(right)

  def split(root: Node): Option[Node] =
    def helper(node: Node): Option[Node] = node match
      case Leaf(value) => Option.when(value >= 10)(node)
      case Pair(left, right) => helper(left).orElse(helper(right))
    helper(root).map(doSplit(root))

  def doSplit(root: Node)(splitter: Node): Node =
    def helper(node: Node): Node = node match
      case Leaf(value) if splitter.eq(node) => Pair(Leaf(value / 2), Leaf(value / 2 + value % 2)) // Careful - strict reference equality
      case Pair(left, right) => Pair(helper(left), helper(right))
      case _ => node
    helper(root)

  def explode(root: Node): Option[Node] =
    def helper(node: Node, depth: Int, path: List[Node]): Option[List[Node]] = node match
      case Leaf(_) => None
      case Pair(Leaf(_), Leaf(_)) if depth == 4 => Some(node :: path)
      case Pair(left, right) => helper(left, depth + 1, node :: path).orElse(helper(right, depth + 1, node :: path))
    helper(root, 0 , Nil).map(doExplode(root))

  def doExplode(root: Node)(exploder: List[Node]): Node =
    val (left, right) = exploder.head match
      case Pair(Leaf(left), Leaf(right)) => (left, right)

    def upLeft(path: List[Node]): Option[Node] = path match
      case child :: Pair(left, _) :: tail if left.ne(child) => Some(downRight(left)) // Careful - strict reference equality
      case child :: Nil => None
      case child :: tail => upLeft(tail)

    def downRight(node: Node): Node = node match
      case Leaf(_) => node
      case Pair(_, right) => downRight(right)

    def upRight(path: List[Node]): Option[Node] = path match
      case child :: Pair(_, right) :: tail if right.ne(child) => Some(downLeft(right)) // Careful - strict reference equality
      case child :: Nil => None
      case child :: tail => upRight(tail)

    def downLeft(node: Node): Node = node match
      case Leaf(_) => node
      case Pair(left, _) => downLeft(left)

    val firstLeft = upLeft(exploder)
    val firstRight = upRight(exploder)

    def helper(node: Node): Node = node match
      case Leaf(value) if firstLeft.exists(_.eq(node)) => Leaf(value + left) // Careful - strict reference equality
      case Leaf(value) if firstRight.exists(_.eq(node)) => Leaf(value + right)
      case Pair(_, _) if exploder.head.eq(node) => Leaf(0)
      case Pair(left, right) => Pair(helper(left), helper(right))
      case _ => node

    helper(root)
  end doExplode

  def add(left: Node, right: Node): Node =
    Iterator.iterate(Option[Node](Pair(left, right))) {
      case Some(state) => explode(state).orElse(split(state))
    }
    .takeWhile(_.isDefined).toSeq.last.get

  def part1(input: Seq[String]): Int = magnitude(input.map(read).reduce(add))

  def part2(input: Seq[String]): Int = input.map(read).toSet.subsets(2)
    .flatMap { set => Seq((set.head, set.last), (set.last, set.head)) }
    .map(add).map(magnitude).max

  def main(args: Array[String]): Unit =
    val data = io.Source.fromResource("AdventOfCode2021/Day18.txt").getLines().toSeq
    println(part1(data))
    println(part2(data))
