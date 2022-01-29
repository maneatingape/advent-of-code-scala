package AdventOfCode2020

object Day04:
  val fourDigits = "\\d{4}".r
  val nineDigits = "\\d{9}".r
  val height = "(\\d+)(cm|in)".r
  val hairColor = "#([0-9]|[a-f]){6}".r
  val eyeColor = "amb|blu|brn|gry|grn|hzl|oth".r

  def byr(value: String): Boolean = fourDigits.matches(value) && value.toInt >= 1920 && value.toInt <= 2002
  def iyr(value: String): Boolean = fourDigits.matches(value) && value.toInt >= 2010 && value.toInt <= 2020
  def eyr(value: String): Boolean = fourDigits.matches(value) && value.toInt >= 2020 && value.toInt <= 2030
  def hgt(value: String): Boolean = value match
    case height(amount, "cm") => amount.toInt >= 150 && amount.toInt <= 193
    case height(amount, "in") => amount.toInt >= 59 && amount.toInt <= 76
    case _ => false
  def hcl(value: String): Boolean = hairColor.matches(value)
  def ecl(value: String): Boolean = eyeColor.matches(value)
  def pid(value: String): Boolean = nineDigits.matches(value)
  def cid(value: String): Boolean = true

  val rules = Map("byr" -> byr, "iyr" -> iyr, "eyr" -> eyr, "hgt" -> hgt, "hcl" -> hcl, "ecl" -> ecl, "pid" -> pid, "cid" -> cid)
  val fields = rules.keySet - "cid"

  def parse(input: String): Seq[Map[String, String]] = input.trim.split("\n\n").toSeq.map { block =>
    block.trim.split("[ \n]+").map(_.split(":")).map(a => a(0) -> a(1)).toMap
  }

  def part1(input: String): Int = parse(input).count(passport => fields.forall(passport.contains))

  def part2(input: String): Int = parse(input).count(passport => fields.forall(passport.contains) && passport.forall(rules(_)(_)))

  def main(args: Array[String]): Unit =
    val data = io.Source.fromResource("AdventOfCode2020/Day04.txt").mkString
    println(part1(data))
    println(part2(data))
