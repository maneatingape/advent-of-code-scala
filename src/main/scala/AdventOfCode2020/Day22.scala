package AdventOfCode2020

object Day22:
  case class Round(player1: Seq[Int], player2: Seq[Int]):
    def player1Wins = Round(player1.tail ++ Seq(player1.head, player2.head), player2.tail)
    def player2Wins = Round(player1.tail, player2.tail ++ Seq(player2.head, player1.head))

  sealed trait Winner
  case class Player1(deck: Seq[Int]) extends Winner
  case class Player2(deck: Seq[Int]) extends Winner

  def parse(input: Seq[String]): Round =
    val index = input.indexOf("")
    val player1 = input.take(index).drop(1).map(_.toInt)
    val player2 = input.drop(index + 2).map(_.toInt)
    Round(player1, player2)

  def score(winner: Winner): Int =
    val deck = winner match
      case Player1(deck) => deck
      case Player2(deck) => deck
    val (score, _) = deck.foldRight((0, 1)) { case (card, (total, multiplier)) =>
      (total + multiplier * card, multiplier + 1)
    }
    score
  end score

  def simpleCombat(round: Round): Winner = round match
    case Round(player1, Seq()) => Player1(player1)
    case Round(Seq(), player2) => Player2(player2)
    case Round(player1, player2) =>
      val nextRound = if player1.head > player2.head then round.player1Wins else round.player2Wins
      simpleCombat(nextRound)
  end simpleCombat

  def recursiveCombat(round: Round, previous: Seq[Round] = Seq()): Winner = round match
    case Round(player1, Seq()) => Player1(player1)
    case Round(Seq(), player2) => Player2(player2)
    case Round(player1, player2) =>
      val nextRound = if previous.contains(round) then
        Round(player1, Seq())
      else if player1.head > player1.tail.size || player2.head > player2.tail.size then
        if player1.head > player2.head then round.player1Wins else round.player2Wins
      else
        val subGame = Round(player1.tail.take(player1.head), player2.tail.take(player2.head))
        recursiveCombat(subGame) match
          case Player1(_) => round.player1Wins
          case Player2(_) => round.player2Wins
      recursiveCombat(nextRound, previous.appended(round))
  end recursiveCombat

  def part1(input: Seq[String]): Int = score(simpleCombat(parse(input)))

  def part2(input: Seq[String]): Int = score(recursiveCombat(parse(input)))

  def main(args: Array[String]): Unit =
    val data = io.Source.fromResource("AdventOfCode2020/Day22.txt").getLines().toSeq
    println(part1(data))
    println(part2(data))
