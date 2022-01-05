package AdventOfCode2018

import scala.annotation.tailrec

object Day13:
  sealed trait Turn
  case object Left extends Turn
  case object Straight extends Turn
  case object Right extends Turn

  case class Point(x: Int, y: Int):
    def delta(dx: Int, dy: Int): Point = Point(x + dx, y + dy)

  case class Cart(tracks: Map[Point, Char], position: Point, facing: Char, turn: Turn):
    def next: Cart =
      val next = position.delta.tupled(nextPosition(facing))
      tracks(next) match
        case '+' => Cart(tracks, next, nextIntersection(facing)(turn), nextTurn(turn))
        case corner @ ('/' | '\\') => Cart(tracks, next, nextCorner(corner)(facing), turn)
        case _ => Cart(tracks, next, facing, turn)

  val nextTurn = Map(Left -> Straight, Straight -> Right, Right -> Left)
  val nextPosition = Map('<' -> (-1, 0), '>' -> (1, 0), '^' -> (0, -1), 'v' -> (0, 1))
  val nextIntersection = Map(
    '<' -> Map(Left -> 'v', Straight -> '<', Right -> '^'),
    '>' -> Map(Left -> '^', Straight -> '>', Right -> 'v'),
    '^' -> Map(Left -> '<', Straight -> '^', Right -> '>'),
    'v' -> Map(Left -> '>', Straight -> 'v', Right -> '<'))
  val nextCorner = Map(
    '/' -> Map('<' -> 'v', '>' -> '^', '^' -> '>', 'v' -> '<'),
    '\\' -> Map('<' -> '^', '>' -> 'v', '^' -> '<', 'v' -> '>'))

  def parse(input: Seq[String]): Seq[Cart] =
    val points = for y <- input.indices; x <- input.head.indices yield Point(x, y) -> input(y)(x)
    val tracks = points.filter(_._2 != ' ').toMap
    points.collect { case (point, facing @ ('<' | '>' | '^' | 'v')) => Cart(tracks, point, facing, Left) }

  def part1(input: Seq[String]): String =
    @tailrec
    def helper(todo: Seq[Cart], carts: Seq[Cart]): Point =
      if todo.isEmpty then
        helper(carts.sortBy(_.position.y).sortBy(_.position.x), Seq())
      else
        val cart = todo.head.next
        if (todo.tail ++ carts).map(_.position).contains(cart.position) then cart.position
        else helper(todo.tail, carts.appended(cart))

    val crash = helper(Seq(), parse(input))
    crash.x + "," + crash.y
  end part1

  def part2(input: Seq[String]): String =
    @tailrec
    def helper(todo: Seq[Cart], carts: Seq[Cart]): Point =
      if todo.size + carts.size == 1 then
        (todo ++ carts).head.next.position
      else if todo.isEmpty then
        helper(carts.sortBy(_.position.y).sortBy(_.position.x), Seq())
      else
        val cart = todo.head.next
        if (todo.tail ++ carts).map(_.position).contains(cart.position) then
          helper(todo.tail.filterNot(_.position == cart.position), carts.filterNot(_.position == cart.position))
        else helper(todo.tail, carts.appended(cart))

    val last = helper(Seq(), parse(input))
    last.x + "," + last.y
  end part2

  def main(args: Array[String]): Unit =
    val data = io.Source.fromResource("AdventOfCode2018/Day13.txt").getLines().toSeq
    println(part1(data))
    println(part2(data))
