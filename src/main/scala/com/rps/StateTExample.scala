import cats.data.StateT
import cats.syntax.all.*
import cats.effect.IO
import cats.effect.IOApp

object StateExampleFlowingValueWithIO {

  type CalculationState = String
  type Calculation[A]   = StateT[IO, CalculationState, A]

  object Calculation {
    def pure[A](a: A): Calculation[A] = StateT.pure[IO, CalculationState, A](a)

    def get: Calculation[CalculationState] = StateT.get[IO, CalculationState]

    def debugPrintLn(value: String): Calculation[Unit] = StateT.liftF(IO.println(value))
  }

  def add(amount: Int): Int => Calculation[Int] = { currentValue =>
    StateT { currentDescription =>
      val newValue       = currentValue + amount
      val newDescription = s"$currentDescription -> add $amount"
      IO.pure((newDescription, newValue))
    }
  }

  def multiplyBy(factor: Int): Int => Calculation[Int] = { currentValue =>
    StateT { currentDescription =>
      val newValue       = currentValue * factor
      val newDescription = s"$currentDescription -> multiplyBy $factor"
      IO.pure((newDescription, newValue))
    }
  }

  val computation: Calculation[Int] = for {
    initialValue    <- Calculation.pure(0)
    valueAfterAdd5  <- add(5)(initialValue)
    _               <- Calculation.debugPrintLn(s"Intermediate value is $valueAfterAdd5")
    valueAfterMult2 <- multiplyBy(2)(valueAfterAdd5)
    finalValue      <- add(3)(valueAfterMult2)
  } yield finalValue

  def runDemo(): IO[Unit] = for {
    initialState: CalculationState <- IO.pure("Start")
    _                              <- IO.println(s"Initial State (Description): '$initialState'")

    resultTuple: (CalculationState, Int)   <- computation.run(initialState)
    (finalDescription, finalComputedResult) = resultTuple

    _ <- IO.println("\nRunning computation...")
    _ <- IO.println(s"Computation Result (final number): $finalComputedResult")
    _ <- IO.println(s"Final State (Description): '$finalDescription'")

  } yield ()
}

object Main3 extends IOApp.Simple {
  def run: IO[Unit] = StateExampleFlowingValueWithIO.runDemo()
}
