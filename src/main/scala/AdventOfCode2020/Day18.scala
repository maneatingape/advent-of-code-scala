package AdventOfCode2020

object Day18:
  enum Parser(pattern: String):
    val regex = pattern.r
    def unapply(line: String): Option[(String, String)] = regex.findPrefixOf(line).map(prefix => line.splitAt(prefix.length))
    case Whitespace extends Parser("\\s+")
    case Natural extends Parser("\\d+")
    case Open extends Parser("\\(")
    case Close extends Parser("\\)")
    case Add extends Parser("\\+")
    case Multiply extends Parser("\\*")

  sealed trait Token
  case class Natural(value: String) extends Token
  case object Open extends Token
  case object Close extends Token
  case object Add extends Token
  case object Multiply extends Token

  def infix(input: String): List[Token] =
    def helper(remaining: String, tokens: List[Token]): List[Token] = remaining match
      case "" => tokens.reverse
      case Parser.Whitespace(head, tail) => helper(tail, tokens)
      case Parser.Natural(head, tail) => helper(tail, Natural(head) :: tokens)
      case Parser.Open(head, tail) => helper(tail, Open :: tokens)
      case Parser.Close(head, tail) => helper(tail, Close :: tokens)
      case Parser.Add(head, tail) => helper(tail, Add :: tokens)
      case Parser.Multiply(head, tail) => helper(tail, Multiply :: tokens)
    helper(input, Nil)

  def postfix(priority: Map[Token, Int])(infix: List[Token]): List[Token] =
    def helper(tokens: List[Token], output: List[Token], stack: List[Token]): List[Token] = tokens match
      case Nil => output.reverse ++ stack
      case ((natural: Natural) :: tail) => helper(tail, natural :: output, stack)
      case (Open :: tail) => helper(tail, output, Open :: stack)
      case (Close :: tail) =>
        if stack.head == Open then helper(tail, output, stack.tail)
        else helper(tokens, stack.head :: output, stack.tail)
      case (operator :: tail) =>
        if stack.isEmpty || priority(stack.head) < priority(operator) then helper(tail, output, operator :: stack)
        else helper(tokens, stack.head :: output, stack.tail)
    helper(infix, Nil, Nil)

  def evaluate(postfix: List[Token]): Long =
    def helper(tokens: List[Token], stack: List[Long]): Long = tokens match
      case Nil => stack.head
      case (Natural(value) :: tail) => helper(tail, value.toLong :: stack)
      case (operator :: tail) => (operator, stack) match
        case (Add, first :: second :: rest) => helper(tail, (first + second) :: rest)
        case (Multiply, first :: second :: rest) => helper(tail, (first * second) :: rest)
        case _ => throw MatchError("Unreachable")
    helper(postfix, Nil)

  def part1(input: Seq[String]): Long =
    val priority = Map(Open -> 0, Multiply -> 1, Add -> 1)
    input.map(infix).map(postfix(priority)).map(evaluate).sum

  def part2(input: Seq[String]): Long =
    val priority = Map(Open -> 0, Multiply -> 1, Add -> 2)
    input.map(infix).map(postfix(priority)).map(evaluate).sum

  def main(args: Array[String]): Unit =
    val data = io.Source.fromResource("AdventOfCode2020/Day18.txt").getLines().toSeq
    println(part1(data))
    println(part2(data))
