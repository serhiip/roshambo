import cats.data.State
import cats.syntax.all.*
import cats.effect.IO
import cats.effect.IOApp

object StateExampleFlowingValue {

  type CalculationState = String
  type Calculation[A]   = State[CalculationState, A]
  object Calculation {
    def startFrom[A](a: A) = State.pure[CalculationState, A](a)
    def get                = State.get[String]
  }

  def add(amount: Int): Int => Calculation[Int] = { currentValue =>
    State { currentDescription =>
      val newValue       = currentValue + amount
      val newDescription = s"$currentDescription -> add $amount"
      (newDescription, newValue)
    }
  }

  def multiplyBy(factor: Int): Int => Calculation[Int] = { currentValue =>
    State { currentDescription =>
      val newValue       = currentValue * factor
      val newDescription = s"$currentDescription -> multiplyBy $factor"
      (newDescription, newValue)
    }
  }

  def getDescription: Calculation[String] =
    State.get[String]

  val computation: Calculation[Int] = for {
    initialValue    <- Calculation.startFrom(0)
    valueAfterAdd5  <- add(5)(initialValue)
    valueAfterMult2 <- multiplyBy(2)(valueAfterAdd5)
    finalValue      <- add(3)(valueAfterMult2)
  } yield finalValue

  def runDemo(): IO[Unit] = for {
    initialState: CalculationState <- IO.pure("Start")
    _                              <- IO.println(s"Initial State (Description): '$initialState'")

    resultTuple: (CalculationState, Int)    = computation.run(initialState).value
    (finalDescription, finalComputedResult) = resultTuple

    _ <- IO.println("Running computation...")
    _ <- IO.println(s"Computation Result (final number): $finalComputedResult")
    _ <- IO.println(s"Final State (Description): '$finalDescription'")

  } yield ()
}

object Main2 extends IOApp.Simple {
  def run: IO[Unit] = StateExampleFlowingValue.runDemo()
}
