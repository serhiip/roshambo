import cats.data.StateT
import cats.syntax.all.*
import cats.Monad
import cats.effect.IO
import cats.effect.IOApp
import cats.effect.std.Console
import cats.Applicative

object StateExampleFlowingValueAbstract {

  type CalculationState     = String
  type Calculation[F[_], A] = StateT[F, CalculationState, A]

  object Calculation {
    def pure[F[_]: Monad, A](a: A): Calculation[F, A] =
      StateT.pure[F, CalculationState, A](a)

    def get[F[_]: Monad]: Calculation[F, CalculationState] =
      StateT.get[F, CalculationState]

    def debugPrintLn[F[_]: Applicative: Console](value: String): Calculation[F, Unit] =
      StateT.liftF(Console[F].println(value))
  }

  def add[F[_]](amount: Int)(using F: Monad[F]): Int => Calculation[F, Int] = { currentValue =>
    StateT { currentDescription =>
      val newValue       = currentValue + amount
      val newDescription = s"$currentDescription -> add $amount"
      F.pure((newDescription, newValue))
    }
  }

  def multiplyBy[F[_]](factor: Int)(using F: Monad[F]): Int => Calculation[F, Int] = { currentValue =>
    StateT { currentDescription =>
      val newValue       = currentValue * factor
      val newDescription = s"$currentDescription -> multiplyBy $factor"
      F.pure((newDescription, newValue))
    }
  }

  def computation[F[_]](using F: Monad[F], console: Console[F]): Calculation[F, Int] = for {
    initialValue    <- Calculation.pure[F, Int](0)
    valueAfterAdd5  <- add(5)(initialValue)
    _               <- Calculation.debugPrintLn(s"Intermediate result is $valueAfterAdd5")
    valueAfterMult2 <- multiplyBy(2)(valueAfterAdd5)
    finalValue      <- add(3)(valueAfterMult2)
  } yield finalValue

  def runComputation[F[_]: Console](
      initialState: CalculationState
  )(using F: Monad[F]): F[(CalculationState, Int)] = {
    computation[F].run(initialState)
  }
}

object Main extends IOApp.Simple {
  import StateExampleFlowingValueAbstract.*

  def run: IO[Unit] = {
    val initialState: CalculationState = "Start"

    for {
      resultTuple                            <- runComputation[IO](initialState)
      (finalDescription, finalComputedResult) = resultTuple

      _ <- IO.println(s"Initial State (Description): '$initialState'")
      _ <- IO.println("\nRunning computation...")
      _ <- IO.println(s"Computation Result (final number): $finalComputedResult")
      _ <- IO.println(s"Final State (Description): '$finalDescription'")
    } yield ()
  }
}
