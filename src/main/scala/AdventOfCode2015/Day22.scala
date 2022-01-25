package AdventOfCode2015

object Day22:
  val MagicMissile = Spell(53, 1)
  val Drain = Spell(73, 1)
  val Shield = Spell(113, 6)
  val Poison = Spell(173, 6)
  val Recharge = Spell(229, 5)
  val spells = Set(MagicMissile, Drain, Shield, Poison, Recharge)

  case class Spell(cost: Int, duration: Int)

  case class State(hero: Int, mana: Int, spent: Int, armor: Int, boss: Int, damage: Int, active: Map[Spell, Int]):
    def applyEffects: State = active.foldLeft(copy(armor = 0)) { case (state, (spell, duration)) =>
      val next = state.effect(spell)
      if duration == 1 then next.copy(active = state.active.removed(spell))
      else next.copy(active = state.active.updated(spell, duration - 1))
    }
    def effect(spell: Spell): State = spell match
      case MagicMissile => copy(boss = boss - 4)
      case Drain => copy(hero = hero + 2, boss = boss - 2)
      case Shield => copy(armor = 7)
      case Poison => copy(boss = boss - 3)
      case Recharge => copy(mana = mana + 101)
    def heroTurn(spell: Spell): State = copy(mana = mana - spell.cost, spent = spent + spell.cost, active = active + (spell -> spell.duration))
    def bossTurn: State = copy(hero = hero - (damage - armor).max(1))
    def hardMode: State = copy(hero = hero - 1)

  def parse(input: Seq[String]): State =
    val Seq(health, damage) = input.map(_.filter(_.isDigit).toInt)
    State(50, 500, 0, 0, health, damage, Map())

  def fight(start: State, hard: Boolean): Int =
    var result = Int.MaxValue

    def helper(state: State, playerTurn: Boolean): Unit =
      val first = if hard && playerTurn then state.hardMode else state
      val second = first.applyEffects

      if first.spent >= result || first.hero <= 0 then ()
      else if second.boss <= 0 then result = result.min(second.spent)
      else if !playerTurn then helper(second.bossTurn, !playerTurn)
      else spells
        .diff(second.active.keySet)
        .filter(_.cost <= second.mana)
        .foreach(spell => helper(second.heroTurn(spell), !playerTurn))
    end helper

    helper(start, true)
    result
  end fight

  def part1(input: Seq[String]): Int = fight(parse(input), false)

  def part2(input: Seq[String]): Int = fight(parse(input), true)

  def main(args: Array[String]): Unit =
    val data = io.Source.fromResource("AdventOfCode2015/Day22.txt").getLines().toSeq
    println(part1(data))
    println(part2(data))
