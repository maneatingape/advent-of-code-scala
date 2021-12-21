package AdventOfCode2021

object Day21:
  val diracDiceThrice: Seq[Int] = for first <- 1 to 3; second <- 1 to 3; third <- 1 to 3 yield first + second + third

  case class Player(space: Int, score: Int = 0):
    def next(move: Int): Player =
      val nextSpace = (((space - 1) + move) % 10) + 1
      Player(nextSpace, score + nextSpace)

  case class State(player1: Player, player2: Player)

  case class DeterministicDice(value: Int = 1, rolled: Int = 0):
    def roll: DeterministicDice = DeterministicDice(value % 100 + 1, rolled + 1)
    def thrice: (DeterministicDice, Int) = Iterator.iterate((this, 0))((dice, move) => (dice.roll, move + dice.value)).drop(3).next()

  case class Total(player1Wins: Long, player2Wins: Long):
    def +(other: Total): Total = Total(player1Wins + other.player1Wins, player2Wins + other.player2Wins)
    def max: Long = player1Wins.max(player2Wins)

  def parse(input: Seq[String]): State =
    val positions = input.map(line => line.drop(1 + line.lastIndexOf(" ")).toInt)
    State(Player(positions(0)), Player(positions(1)))

  def part1(input: Seq[String]): Int =
    def play(state: State, dice: DeterministicDice): Int =
      val State(player1, player2) = state

      val (dice1, player1Move) = dice.thrice
      val nextPlayer1 = player1.next(player1Move)

      val (dice2, player2Move) = dice1.thrice
      val nextPlayer2 = player2.next(player2Move)

      if nextPlayer1.score >= 1000 then player2.score * dice1.rolled
      else if nextPlayer2.score >= 1000 then nextPlayer1.score * dice2.rolled
      else play(State(nextPlayer1, nextPlayer2), dice2)
    end play

    play(parse(input), DeterministicDice())
  end part1

  def part2(input: Seq[String]): Long =
    val cache = collection.mutable.Map[State, Total]()
    def play(state: State): Total = cache.getOrElseUpdate(state, compute(state))
    def partition(player: Player) = diracDiceThrice.map(player.next).partition(_.score >= 21)

    def compute(state: State): Total =
      val (winsP1, rest) = partition(state.player1)
      rest.map { nextP1 =>
        val (winsP2, other) = partition(state.player2)
        other.map(nextP2 => play(State(nextP1, nextP2))).fold(Total(0, winsP2.size))(_ + _)
      }
      .fold(Total(winsP1.size, 0))(_ + _)
    end compute

    play(parse(input)).max
  end part2

  def main(args: Array[String]): Unit =
    val data = io.Source.fromResource("AdventOfCode2021/Day21.txt").getLines().toSeq
    println(part1(data))
    println(part2(data))
