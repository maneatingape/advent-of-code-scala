package AdventOfCode2021

object Day23:
  val costPerSpace = Map('A' -> 1, 'B' -> 10, 'C' -> 100, 'D' -> 1000)
  val roomIndex = Map('A' -> 2, 'B' -> 4, 'C' -> 6, 'D' -> 8)

  case class Burrow(roomMax: Int, cost: Int, hallway: Seq[Char], rooms: Map[Char, Seq[Char]]):
    def finished: Boolean = rooms.forall((kind, room) => room.size == roomMax && room.forall(_ == kind))

  def small(input: Seq[String]): Burrow =
    def room(column: Int) = Seq(input(2)(column), input(3)(column))
    Burrow(2, 0, Seq.fill(11)('.'), Map('A' -> room(3), 'B' -> room(5), 'C' -> room(7), 'D' -> room(9)))

  def large(input: Seq[String]): Burrow =
    def room(column: Int, second: Char, third: Char) = Seq(input(2)(column), second, third, input(3)(column))
    Burrow(4, 0, Seq.fill(11)('.'), Map('A' -> room(3, 'D', 'D'), 'B' -> room(5, 'C', 'B'), 'C' -> room(7, 'B', 'A'), 'D' -> room(9, 'A', 'C')))

  def paths(current: Burrow): Seq[Burrow] =
    val first = current.hallway.zipWithIndex.filter(_._1 != '.').flatMap(hallwayToRoom(current))
    val second = current.rooms.flatMap(roomToRoom(current))
    val preferred = first ++ second
    if preferred.nonEmpty then preferred else current.rooms.flatMap(roomToHallway(current)).toSeq

  def hallwayToRoom(current: Burrow)(amphipod: Char, start: Int): Option[Burrow] =
    val end = roomIndex(amphipod)
    val range = if start < end then ((start + 1) to end) else (end to (start - 1))

    if current.rooms(amphipod).forall(_ == amphipod) && range.forall(n => current.hallway(n) == '.') then
      val nextH = current.hallway.updated(start, '.')
      val nextR = current.rooms.updated(amphipod, current.rooms(amphipod).prepended(amphipod))
      val nextC = current.cost + (range.size + current.roomMax - current.rooms(amphipod).size) * costPerSpace(amphipod)
      Some(Burrow(current.roomMax, nextC, nextH, nextR))
    else None
  end hallwayToRoom

  def roomToRoom(current: Burrow)(key: Char, room: Seq[Char]): Seq[Burrow] =
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

  def roomToHallway(current: Burrow)(key: Char, room: Seq[Char]): Seq[Burrow] =
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

  def shuffle(burrow: Burrow): Option[Int] =
    def move(current: Burrow, energy: Option[Int]): Option[Int] =
      if current.finished then Some(current.cost)
      else if energy.exists(_ < current.cost) then None
      else paths(current).map(move(_, energy)).flatten.minOption
    move(burrow, None)

  def part1(input: Seq[String]): Int = shuffle(small(input)).get

  def part2(input: Seq[String]): Int = shuffle(large(input)).get

  def main(args: Array[String]): Unit =
    val data = io.Source.fromResource("AdventOfCode2021/Day23.txt").getLines().toSeq
    println(part1(data))
    println(part2(data))
