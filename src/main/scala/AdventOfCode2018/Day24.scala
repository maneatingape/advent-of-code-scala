package AdventOfCode2018

import scala.annotation.tailrec

object Day24:
  sealed trait Army
  case object Draw extends Army
  case object Immune extends Army
  case object Infection extends Army

  val groupRegex = """(\d+) units each with (\d+) hit points (?:.*)with an attack that does (\d+) (\w+) damage at initiative (\d+)""".r
  val immuneRegex = """immune to (.+?)[;|\)]""".r.unanchored
  val weakRegex = """weak to (.+?)[;|\)]""".r.unanchored

  case class Attribute(hp: Int, initiative: Int)
  case class Defense(immune: Set[String], weak: Set[String])
  case class Attack(damage: Int, kind: String)

  case class Group(army: Army, attribute: Attribute, defense: Defense, attack: Attack)(var units: Int):
    def effectivePower: Int = units * attack.damage
    def computeDamage(attacker: Group): Int =
      if defense.immune.contains(attacker.attack.kind) then 0
      else if defense.weak.contains(attacker.attack.kind) then 2 * attacker.effectivePower
      else attacker.effectivePower

  def parse(input: Seq[String], boost: Int): Seq[Group] =
    val index = input.indexOf("")
    val immune = input.slice(1, index).map(parseGroup(Immune, boost, _))
    val infection = input.drop(index + 2).map(parseGroup(Infection, 0, _))
    immune ++ infection

  def parseGroup(army: Army, boost: Int, line: String): Group =
    val immune = line match
      case immuneRegex(items) => items.split(", ").toSet
      case _ => Set()
    val weak = line match
      case weakRegex(items) => items.split(", ").toSet
      case _ => Set()
    line match
      case groupRegex(units, hp, damage, kind, initiative) =>
        val attribute = Attribute(hp.toInt, initiative.toInt)
        val defense = Defense(immune, weak)
        val attack = Attack(damage.toInt + boost, kind)
        Group(army, attribute, defense, attack)(units.toInt)

  @tailrec
  def fight(groups: Seq[Group]): (Army, Int) =
    val previous = groups.map(_.units).sum

    val targetSelection = groups.sortBy(group => (-group.effectivePower, -group.attribute.initiative))
    val targets = targetSelection.foldLeft(Map.empty[Group, Group]) { (targets, next) =>
      val candidate = groups
        .filterNot(targets.contains)
        .filter(_.army != next.army)
        .filter(_.units > 0)
        .filter(_.computeDamage(next) > 0)
        .maxByOption(target => (target.computeDamage(next), target.effectivePower, target.attribute.initiative))

      candidate match
        case Some(target) => targets.updated(target, next)
        case None => targets
    }

    targets.toSeq.sortBy((target, attacker) => -attacker.attribute.initiative).foreach { (target, attacker) =>
      target.units = (target.units - target.computeDamage(attacker) / target.attribute.hp).max(0)
    }

    if groups.map(_.units).sum == previous then (Draw, -1)
    else if groups.filter(_.units > 0).map(_.army).toSet.size == 1 then (groups.filter(_.units > 0).head.army, groups.map(_.units).sum)
    else fight(groups)
  end fight

  def part1(input: Seq[String]): Int =
    val groups = parse(input, 0)
    val (winner, units) = fight(groups)
    units

  def part2(input: Seq[String]): Int =
    @tailrec
    def helper(boost: Int): Int =
      val groups = parse(input, boost)
      val (winner, units) = fight(groups)
      if winner == Immune then units else helper(boost + 1)

    helper(1)
  end part2

  def main(args: Array[String]): Unit =
    val data = io.Source.fromResource("AdventOfCode2018/Day24.txt").getLines().toSeq
    println(part1(data))
    println(part2(data))
