package AdventOfCode2020

object Day12:
  case class Ship(x: Int = 0, y: Int = 0, angle: Int = 0, wx: Int = 10, wy: Int = 1)

  def cos(angle: Int) = angle % 360 match
    case 0 => 1
    case 180 | -180 => -1
    case _ => 0

  def sin(angle: Int) = angle % 360 match
    case 90 | -270 => 1
    case 270 | -90 => -1
    case _ => 0

  def parse(line: String): (Char, Int) = line.head -> line.tail.toInt

  def part1(input: Seq[String]): Int =
    val ship = input.map(parse).foldLeft(Ship()) { case (ship, (command, value)) => command match
      case 'N' => ship.copy(y = ship.y + value)
      case 'S' => ship.copy(y = ship.y - value)
      case 'E' => ship.copy(x = ship.x + value)
      case 'W' => ship.copy(x = ship.x - value)
      case 'L' => ship.copy(angle = ship.angle + value)
      case 'R' => ship.copy(angle = ship.angle - value)
      case 'F' => ship.copy(x = ship.x + value * cos(ship.angle), y = ship.y + value * sin(ship.angle))
    }
    ship.x.abs + ship.y.abs

  def part2(input: Seq[String]): Int =
    val ship = input.map(parse).foldLeft(Ship()) { case (ship, (command, value)) => command match
      case 'N' => ship.copy(wy = ship.wy + value)
      case 'S' => ship.copy(wy = ship.wy - value)
      case 'E' => ship.copy(wx = ship.wx + value)
      case 'W' => ship.copy(wx = ship.wx - value)
      case 'L' => ship.copy(wx = ship.wx * cos(value) - ship.wy * sin(value), wy = ship.wy * cos(value) + ship.wx * sin(value))
      case 'R' => ship.copy(wx = ship.wx * cos(value) + ship.wy * sin(value), wy = ship.wy * cos(value) - ship.wx * sin(value))
      case 'F' => ship.copy(x = ship.x + value * ship.wx, y = ship.y + value * ship.wy)
    }
    ship.x.abs + ship.y.abs

  def main(args: Array[String]): Unit =
    val data = io.Source.fromResource("AdventOfCode2020/Day12.txt").getLines().toSeq
    println(part1(data))
    println(part2(data))
