package AdventOfCode2021

object Day23:
  val costPerSpace = Map('A' -> 1, 'B' -> 10, 'C' -> 100, 'D' -> 1000)
  val roomIndex = Map('A' -> 2, 'B' -> 4, 'C' -> 6, 'D' -> 8)

  case class Burrow(roomMax: Int, cost: Int, hallway: Seq[Char], rooms: Map[Char, Seq[Char]])

  def small(input: Seq[String]): Burrow =
    val hallway = Seq.fill(11)('.')
    val roomA = Seq(input(2)(3), input(3)(3))
    val roomB = Seq(input(2)(5), input(3)(5))
    val roomC = Seq(input(2)(7), input(3)(7))
    val roomD = Seq(input(2)(9), input(3)(9))
    Burrow(2, 0, hallway, Map('A' -> roomA, 'B' -> roomB, 'C' -> roomC, 'D' -> roomD))
  end small

  def large(input: Seq[String]): Burrow =
    val hallway = Seq.fill(11)('.')
    val roomA = Seq(input(2)(3), 'D', 'D', input(3)(3))
    val roomB = Seq(input(2)(5), 'C', 'B', input(3)(5))
    val roomC = Seq(input(2)(7), 'B', 'A', input(3)(7))
    val roomD = Seq(input(2)(9), 'A', 'C', input(3)(9))
    Burrow(4, 0, hallway, Map('A' -> roomA, 'B' -> roomB, 'C' -> roomC, 'D' -> roomD))
  end large

  def debug(current: Burrow): Unit =
    def extend(room: Seq[Char]) = (Seq.fill(current.roomMax)('.') ++ room).takeRight(current.roomMax)
    val extended = current.rooms.view.mapValues(extend)
    println("Cost: " + current.cost)
    println(current.hallway.mkString)
    for i <- 0 until current.roomMax do
      println("  " + extended('A')(i) + " " + extended('B')(i) + " " + extended('C')(i) + " " + extended('D')(i))
    println()
  end debug

  def paths(current: Burrow): Seq[Burrow] =
    val third = current.rooms.flatMap { case (key, room) =>
      roomToHallway(current, key, room)
    }
    val second = current.rooms.flatMap { case (key, room) =>
      roomToRoom(current, key, room)
    }
    val first = current.hallway.zipWithIndex.filter(_._1 != '.').map { (amphipod, index) =>
      hallwayToRoom(current, amphipod, index)
    }.flatten

    val preferred = first.toSeq ++ second
    if preferred.nonEmpty then preferred else third.toSeq
  end paths

  def roomToHallway(current: Burrow, key: Char, room: Seq[Char]): Seq[Burrow] =
    if room.forall(_ == key) then return Seq()

    val index = roomIndex(key)
    val valid = Seq(0, 1, 3, 5, 7, 9, 10)
    val left = valid.filter(_ < index).reverse.takeWhile(n => current.hallway(n) == '.')
    val right = valid.filter(_ > index).takeWhile(n => current.hallway(n) == '.')

    (left.reverse ++ right).map { pos =>
      val nextH = current.hallway.updated(pos, room.head)
      val nextR = current.rooms.updated(key, room.tail)
      val nextC = current.cost + ((pos - index).abs + 1 + current.roomMax - room.size) * costPerSpace(room.head)
      Burrow(current.roomMax, nextC, nextH, nextR)
    }
  end roomToHallway

  def hallwayToRoom(current: Burrow, amphipod: Char, start: Int): Option[Burrow] =
    val end = roomIndex(amphipod)
    val range = if start < end then ((start + 1) to end) else (end to (start - 1))

    if current.rooms(amphipod).forall(_ == amphipod) && range.forall(n => current.hallway(n) == '.') then
      val nextH = current.hallway.updated(start, '.')
      val nextR = current.rooms.updated(amphipod, current.rooms(amphipod).prepended(amphipod))
      val nextC = current.cost + (range.size + current.roomMax - current.rooms(amphipod).size) * costPerSpace(amphipod)
      Some(Burrow(current.roomMax, nextC, nextH, nextR))
    else None
  end hallwayToRoom

  def roomToRoom(current: Burrow, key: Char, room: Seq[Char]): Seq[Burrow] =
    if room.forall(_ == key) then return Seq()

    val start = roomIndex(key)
    val end = roomIndex(room.head)
    val range = if start < end then (start to end) else (end to start)

    if current.rooms(room.head).forall(_ == room.head) && range.forall(n => current.hallway(n) == '.') then
      val nextR = current.rooms
        .updated(key, room.tail)
        .updated(room.head, current.rooms(room.head).prepended(room.head))
      val nextC = current.cost + (range.size + current.roomMax - current.rooms(key).size + current.roomMax - current.rooms(room.head).size) * costPerSpace(room.head)
      Seq(Burrow(current.roomMax, nextC, current.hallway, nextR))
    else Seq()
  end roomToRoom

  def isFinished(burrow: Burrow): Boolean =
    burrow.rooms.forall { case (key, room) => room.size == burrow.roomMax && room.forall(_ == key) }

  def shuffle(burrow: Burrow): Int =
    var total = Int.MaxValue

    def move(current: Burrow): Unit =
      if isFinished(current) then total = total.min(current.cost)
      else if current.cost < total then paths(current).foreach(move)
    end move

    move(burrow)
    total
  end shuffle

  def part1(input: Seq[String]): Int = shuffle(small(input))

  def part2(input: Seq[String]): Int = shuffle(large(input))

  def main(args: Array[String]): Unit =
    val data = io.Source.fromResource("AdventOfCode2021/Day23.txt").getLines().toSeq
    println(part1(data))
    println(part2(data))
