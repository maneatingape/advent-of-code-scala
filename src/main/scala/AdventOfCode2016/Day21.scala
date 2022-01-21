package AdventOfCode2016

object Day21:
  val swapIndex = "swap position (\\d+) with position (\\d+)".r
  val swapLetter = "swap letter (\\w) with letter (\\w)".r
  val rotateLeft = "rotate left (\\d+) steps?".r
  val rotateRight = "rotate right (\\d+) steps?".r
  val rotateLetter = "rotate based on position of letter (\\w)".r
  val reverse = "reverse positions (\\d+) through (\\d+)".r
  val move = "move position (\\d+) to position (\\d+)".r

  def doSwapIndex(password: String, x: Int, y: Int): String =
    password.updated(x, password(y)).updated(y, password(x))

  def doSwapLetter(password: String, a: String, b: String): String =
    val (x, y) = (password.indexOf(a), password.indexOf(b))
    password.updated(x, password(y)).updated(y, password(x))

  def doRotateLeft(password: String, n: Int): String =
    password.drop(n) + password.take(n)

  def doRotateRight(password: String, n: Int): String =
    password.takeRight(n) + password.dropRight(n)

  def doRotateLetterRight(password: String, a: String): String =
    val index = password.indexOf(a)
    val n = if index >= 4 then (index + 2) % password.size else (index + 1) % password.size
    password.takeRight(n) + password.dropRight(n)

  def doRotateLetterLeft(password: String, a: String): String =
    Iterator.iterate(password)(doRotateLeft(_, 1)).dropWhile(next => doRotateLetterRight(next, a) != password).next()

  def doReverse(password: String, x: Int, y: Int): String =
    password.patch(x, password.slice(x, y + 1).reverse, y + 1 - x)

  def doMove(password: String, x: Int, y: Int): String =
    password.patch(x, "", 1).patch(y, "" + password(x), 0)

  def part1(input: Seq[String], password: String): String = input.foldLeft(password) { (password, op) =>
    op match
      case swapIndex(x, y) => doSwapIndex(password, x.toInt, y.toInt)
      case swapLetter(a, b) => doSwapLetter(password, a, b)
      case rotateLeft(n) => doRotateLeft(password, n.toInt)
      case rotateRight(n) => doRotateRight(password, n.toInt)
      case rotateLetter(a) => doRotateLetterRight(password, a)
      case reverse(x, y) => doReverse(password, x.toInt, y.toInt)
      case move(x, y) => doMove(password, x.toInt, y.toInt)
  }

  def part2(input: Seq[String], password: String): String = input.foldRight(password) { (op, password) =>
    op match
      case swapIndex(x, y) => doSwapIndex(password, x.toInt, y.toInt)
      case swapLetter(a, b) => doSwapLetter(password, a, b)
      case rotateLeft(n) => doRotateRight(password, n.toInt)
      case rotateRight(n) => doRotateLeft(password, n.toInt)
      case rotateLetter(a) => doRotateLetterLeft(password, a)
      case reverse(x, y) => doReverse(password, x.toInt, y.toInt)
      case move(x, y) => doMove(password, y.toInt, x.toInt)
  }

  def main(args: Array[String]): Unit =
    val data = io.Source.fromResource("AdventOfCode2016/Day21.txt").getLines().toSeq
    println(part1(data, "abcdefgh"))
    println(part2(data, "fbgdceah"))
