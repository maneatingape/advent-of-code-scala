package AdventOfCode2015

object Day21:
  case class Item(cost: Int, damage: Int, armor: Int):
    def +(other: Item): Item = Item(cost + other.cost, damage + other.damage, armor + other.armor)

  val none = Item(0, 0, 0)
  val weapons = Set(Item(8, 4, 0), Item(10, 5, 0), Item(25, 6, 0), Item(40, 7, 0), Item(74, 8, 0))
  val armors = Set(none, Item(13, 0, 1), Item(31, 0, 2), Item(53, 0, 3), Item(75, 0, 4), Item(102, 0, 5))
  val rings = Seq(none, none, Item(25, 1, 0), Item(50, 2, 0), Item(100, 3, 0), Item(20, 0, 1), Item(40, 0, 2), Item(80, 0, 3))
    .combinations(2).map(c => c.head + c.last).toSet
  val outfits = for weapon <- weapons; armor <- armors; ring <- rings yield weapon + armor + ring

  def parse(input: Seq[String]): Item =
    val Seq(health, damage, armor) = input.map(_.filter(_.isDigit).toInt)
    Item(health, damage, armor)

  def fight(boss: Item)(hero: Item): Boolean =
    val heroTurns = boss.cost / (hero.damage - boss.armor).max(1)
    val bossTurns = 100 / (boss.damage - hero.armor).max(1)
    heroTurns <= bossTurns

  def part1(input: Seq[String]): Int =
    val boss = parse(input)
    outfits.filter(fight(boss)).map(_.cost).min

  def part2(input: Seq[String]): Int =
    val boss = parse(input)
    outfits.filterNot(fight(boss)).map(_.cost).max

  def main(args: Array[String]): Unit =
    val data = io.Source.fromResource("AdventOfCode2015/Day21.txt").getLines().toSeq
    println(part1(data))
    println(part2(data))
