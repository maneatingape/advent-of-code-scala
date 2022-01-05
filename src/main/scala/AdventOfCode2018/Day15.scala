package AdventOfCode2018

import scala.annotation.tailrec

object Day15:
  case class Point(x: Int, y: Int):
    def delta(dx: Int, dy: Int): Point = Point(x + dx, y + dy)
    def orthogonal: Seq[Point] = Seq((0, -1), (-1, 0), (1, 0), (0, 1)).map(delta) // IMPORTANT: In reading order!

  case class Creature(position: Point, kind: Char, health: Int, attack: Int, turn: Int)

  given readingOrder: Ordering[Point] = Ordering.by(point => (point.y, point.x))
  val turnOrder: Ordering[Creature] = Ordering.by(creature => (creature.turn, creature.position))
  val attackOrder: Ordering[Creature] = Ordering.by(creature => (creature.health, creature.position))

  def parse(input: Seq[String], elfAttackPower: Int): (Set[Point], Set[Creature]) =
    val points = for y <- input.indices; x <- input.head.indices yield Point(x, y) -> input(y)(x)
    val walls = points.collect { case (point, '#') => point }.toSet
    val elves = points.collect { case (point, 'E') => Creature(point, 'E', 200, elfAttackPower, 0) }.toSet
    val goblins = points.collect { case (point, 'G') => Creature(point, 'G', 200, 3, 0) }.toSet
    (walls, elves ++ goblins)

  @tailrec
  def fight(walls: Set[Point], creatures: Set[Creature]): (Char, Int, Int) =
    if creatures.map(_.kind).size == 1 then
      val winner = creatures.head.kind
      val survivors = creatures.size
      val score = creatures.map(_.turn).min * creatures.toSeq.map(_.health).sum
      (winner, survivors, score)
    else
      val current = creatures.min(turnOrder)
      val obstacles = walls ++ creatures.map(_.position)
      val enemies = creatures.filter(_.kind != current.kind).map(creature => creature.position -> creature).toMap
      val moved = move(current, enemies, obstacles)
      val attacked = attack(moved, enemies, creatures)
      fight(walls, attacked - current + moved)

  def move(creature: Creature, enemies: Map[Point, Creature], obstacles: Set[Point]): Creature =
    if creature.position.orthogonal.exists(enemies.contains) then
      creature.copy(turn = creature.turn + 1)
    else
      val candidates = enemies.keySet.flatMap(_.orthogonal)
      bfs(creature.position, candidates, obstacles) match
        case Some(point) => creature.copy(position = point, turn = creature.turn + 1)
        case None => creature.copy(turn = creature.turn + 1)

  def attack(creature: Creature, enemies: Map[Point, Creature], creatures: Set[Creature]): Set[Creature] =
    creature.position.orthogonal.flatMap(enemies.get).minOption(attackOrder) match
      case Some(enemy) =>
        val nextHealth = enemy.health - creature.attack
        if nextHealth <= 0 then creatures - enemy else creatures - enemy + enemy.copy(health = nextHealth)
      case None => creatures

  def bfs(start: Point, candidates: Set[Point], obstacles: Set[Point]): Option[Point] =
    @tailrec
    def helper(todo: Seq[Point], paths: Map[Point, Seq[Point]], found: Option[Seq[Point]]): Option[Point] =
      if todo.isEmpty then found.map(_.head) else
        val point = todo.head
        val cost = paths(point).size

        val (nextTodo, nextPaths) = point.orthogonal
          .filterNot(obstacles.contains)
          .filter(next => found.forall(_.size > cost + 1))
          .filter(next => paths.get(next).forall(_.size > cost + 1))
          .foldLeft((todo.tail, paths)) { case ((todo, paths), next) =>
            (todo.appended(next), paths.updated(next, paths(point).appended(next)))
          }

        val nextFound = if !candidates.contains(point) then found else found match
          case None => Some(paths(point))
          case Some(other) => if cost < other.size then Some(paths(point)) else Some(other)

        helper(nextTodo, nextPaths, nextFound)
    end helper

    helper(Seq(start), Map(start -> Seq()), None)
  end bfs

  def part1(input: Seq[String]): Int =
    val (walls, creatures) = parse(input, 3)
    val (_, _, score) = fight(walls, creatures)
    score

  def part2(input: Seq[String]): Int =
    def helper(elfAttackPower: Int): Int =
      val (walls, creatures) = parse(input, elfAttackPower)
      val (winner, survivors, score) = fight(walls, creatures)
      if winner == 'E' && survivors == creatures.count(_.kind == 'E') then score
      else helper(elfAttackPower + 1)

    helper(3)
  end part2

  def main(args: Array[String]): Unit =
    val data = io.Source.fromResource("AdventOfCode2018/Day15.txt").getLines().toSeq
    println(part1(data))
    println(part2(data))
