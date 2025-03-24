package com.rps

import cats.effect.IOApp
import cats.effect.IO
import cats.effect.std.Console
import cats.Monad
import cats.syntax.all.*
import cats.Show
import javax.sound.sampled.Clip
import cats.Applicative
import cats.Functor

enum Move:
  case Rock, Paper, Scissors

object Move {
  enum ReadError:
    case InvalidMove(input: String)

  def fromString(in: String): Either[ReadError, Move] = in match {
    case "rock" | "r"     => Rock.asRight[ReadError]
    case "paper" | "p"    => Paper.asRight[ReadError]
    case "scissors" | "s" => Scissors.asRight[ReadError]
    case other            => ReadError.InvalidMove(other).asLeft[Move]
  }

  given Show[ReadError] = Show[String].contramap[ReadError] { case ReadError.InvalidMove(input) =>
    s"Specified input $input is not valid input. Valid inputs are: rock, paper, scissors (r, p, s)"
  }
}

enum Result:
  case Win[F[_]](winner: Player[F])
  case Tie

object Result {

  implicit val showForResult: Show[Result] = Show[String].contramap[Result] {
    case Win(p) => s"${p.name} won this round"
    case Tie    => s"There was a tie!"
  }
}

sealed abstract trait Player[F[_]] {

  val name: String

  def getMove(): F[Move]
}

sealed abstract trait RockPaperScissors[F[_]] {
  def round(player1: Player[F], player2: Player[F]): F[Result]
}

final class CliPlayer[F[_]: Console: Monad](val name: String) extends Player[F] {

  override def getMove(): F[Move] = for {
    _      <- Console[F].println(s"Your next move $name: ")
    move   <- Console[F].readLine.map(Move.fromString)
    result <-
      move.fold(error => Console[F].println(error.show) >> getMove(), _.pure[F])
  } yield result

}

final class CliRockPaperScissors[F[_]: Console: Monad] extends RockPaperScissors[F] {

  override def round(player1: Player[F], player2: Player[F]): F[Result] = for {
    choiceOne <- player1.getMove()
    choiceTwo <- player2.getMove()

    result = (choiceOne, choiceTwo) match {
               case Move.Rock -> Move.Rock         => Result.Tie
               case Move.Scissors -> Move.Scissors => Result.Tie
               case Move.Paper -> Move.Paper       => Result.Tie
               case Move.Rock -> Move.Scissors     => Result.Win(player1)
               case Move.Scissors -> Move.Paper    => Result.Win(player1)
               case Move.Paper -> Move.Rock        => Result.Win(player1)
               case Move.Scissors -> Move.Rock     => Result.Win(player2)
               case Move.Paper -> Move.Scissors    => Result.Win(player2)
               case Move.Rock -> Move.Paper        => Result.Win(player2)
             }
  } yield result
}

final class BestOfNGame[F[_]: Monad: Console](baseGame: RockPaperScissors[F], totalGames: Int) {
  var results = List.empty[Result]

  def rounds(player1: Player[F], player2: Player[F]): F[Result] = {
    baseGame
      .round(player1: Player[F], player2: Player[F])
      .replicateA(totalGames)
      .map(games => results = games)
      .flatMap(_ => printRounds)
      .map(_ => computeResult(player1, player2))
  }

  private def computeResult(player1: Player[F], player2: Player[F]): Result = {
    var player1WinCount = 0
    var player2WinCount = 0
    results foreach {
      case Result.Win(p) => if p == player1 then player1WinCount += 1 else player2WinCount += 1
      case Result.Tie    => ()
    }
    if player1WinCount > player2WinCount then Result.Win(player1)
    else if player2WinCount > player1WinCount then Result.Win(player2)
    else Result.Tie
  }

  private def printRounds(using Console[F]): F[Unit] = {
    results.zipWithIndex.traverse_ { case (result, index) =>
      Console[F].println(s"Round ${index + 1}: ${result.show}")
    }
  }
}

object Main extends IOApp.Simple {

  def run: IO[Unit] = {
    val player1  = new CliPlayer[IO]("Bob")
    val player2  = new CliPlayer[IO]("Alice")
    val baseGame = new CliRockPaperScissors[IO]
    val game     = new BestOfNGame(baseGame, totalGames = 3)

    for {
      result <- game.rounds(player1, player2)
      _      <- IO.println(result.show)
    } yield ()
  }
}
