package AdventOfCode2022

object Day19:
  case class Resources(ore: Int, clay: Int, obsidian: Int, geode: Int):
    def +(r: Resources): Resources = Resources(ore + r.ore, clay + r.clay, obsidian + r.obsidian, geode + r.geode)
    def -(r: Resources): Resources = Resources(ore - r.ore, clay - r.clay, obsidian - r.obsidian, geode - r.geode)
    def <=(r: Resources): Boolean = ore <= r.ore && clay <= r.clay && obsidian <= r.obsidian && geode <= r.geode

  def maximize(input: Seq[String], minutes: Int): Seq[Int] = input.map { line =>
    val Seq(id, ore1, ore2, ore3, clay, ore4, obsidian) = line.split("\\D+").tail.map(_.toInt).toSeq

    val oreBotCost = Resources(ore1, 0, 0, 0)
    val clayBotCost = Resources(ore2, 0, 0, 0)
    val obsidianBotCost = Resources(ore3, clay, 0, 0)
    val geodeBotCost = Resources(ore4, 0, obsidian, 0)

    val zero = Resources(0, 0, 0, 0)
    val oreBot = Resources(1, 0, 0, 0)
    val clayBot = Resources(0, 1, 0, 0)
    val obsidianBot = Resources(0, 0, 1, 0)
    val geodeBot = Resources(0, 0, 0, 1)

    val maxOre = ore1.max(ore2).max(ore3).max(ore4)
    val maxClay = clay
    val maxObsidian = obsidian

    def helper(time: Int, bots: Resources, resources: Resources, prevCanOre: Boolean, prevCanClay: Boolean, prevCanObsidian: Boolean): Int =
      if time == 0 then
        resources.geode
      else if geodeBotCost <= resources then
        helper(time - 1, bots + geodeBot, resources + bots - geodeBotCost, false, false, false)
      else
        val canOre = oreBotCost <= resources && bots.ore < maxOre
        val canClay = clayBotCost <= resources && bots.clay < maxClay
        val canObsidian = obsidianBotCost <= resources && bots.obsidian < maxObsidian

        val first = helper(time - 1, bots, resources + bots, canOre, canClay, canObsidian)
        val second = if canOre && !prevCanOre then helper(time - 1, bots + oreBot, resources + bots - oreBotCost, false, false, false) else 0
        val third = if canClay && !prevCanClay then helper(time - 1, bots + clayBot, resources + bots - clayBotCost, false, false, false) else 0
        val fourth = if canObsidian && !prevCanObsidian then helper(time - 1, bots + obsidianBot, resources + bots - obsidianBotCost, false, false, false) else 0

        first.max(second).max(third).max(fourth)
      end if
    end helper

    id * helper(minutes, oreBot, zero, false, false, false)
  }

  def part1(input: Seq[String]): Int = maximize(input, 24).sum

  def part2(input: Seq[String]): Int = maximize(input.take(3), 32).product / 6

  def main(args: Array[String]): Unit =
    val data = io.Source.fromResource("AdventOfCode2022/Day19.txt").getLines().toSeq
    println(part1(data))
    println(part2(data))
