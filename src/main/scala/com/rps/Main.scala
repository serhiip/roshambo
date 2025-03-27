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
import cats.data.StateT

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

  type GameState[A] = StateT[F, List[Result], A]

  private def playRound(player1: Player[F], player2: Player[F]): GameState[Result] =
    for {
      roundResult <- StateT.liftF(baseGame.round(player1, player2))
      _           <- StateT.modify[F, List[Result]](_ :+ roundResult)
    } yield roundResult

  def rounds(player1: Player[F], player2: Player[F]): F[Result] = {
    val game = for {
      allRoundResults <- playRound(player1, player2).replicateA(totalGames)
      _               <- printRounds
      finalResult     <- computeResult(player1, player2)
    } yield finalResult

    game.runA(List.empty)
  }

  private def computeResult(player1: Player[F], player2: Player[F]): GameState[Result] = for {
    results <- StateT.get[F, List[Result]]
    scores   = results.foldLeft((0, 0)) {
                 case ((p1Wins, p2Wins), Result.Win(p)) if p == player1 => (p1Wins + 1, p2Wins)
                 case ((p1Wins, p2Wins), Result.Win(p)) if p == player2 => (p1Wins, p2Wins + 1)
                 case (wins, Result.Tie)                                => wins
                 case (wins, _)                                         => wins
               }

    (player1WinCount, player2WinCount) = scores
  } yield
    if (player1WinCount > player2WinCount) Result.Win(player1)
    else if (player2WinCount > player1WinCount) Result.Win(player2)
    else Result.Tie

  private def printRounds(using C: Console[F]): GameState[Unit] =
    for {
      results <- StateT.get[F, List[Result]]
      _       <- StateT.liftF(
                   results.zipWithIndex.traverse_ { case (result, index) =>
                     C.println(s"Round ${index + 1}: ${result.show}")
                   }
                 )
    } yield ()
}

object Main extends IOApp.Simple {

  def run: IO[Unit] = {
    val player1  = new CliPlayer[IO]("Bob")
    val player2  = new CliPlayer[IO]("Alice")
    val baseGame = new CliRockPaperScissors[IO]
    val game     = new BestOfNGame(baseGame, totalGames = 3)

    for {
      result <- game.rounds(player1, player2)
      _      <- IO.println(result.show + " (and the game)")
    } yield ()
  }
}
