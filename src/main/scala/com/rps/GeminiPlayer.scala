package com.rps

import cats.effect.std.Console
import cats.syntax.all.*
import com.google.genai.Client

import scala.jdk.CollectionConverters.*
import cats.effect.kernel.Sync
import cats.ApplicativeThrow
import com.google.genai.types.GenerateContentConfig

val projectId = sys.env("PROJECT_ID") // Replace with your project ID
val location  = "europe-west1"        // Replace with your location
val modelName = "gemini-2.0-flash"    // Or your preferred model

private class GeminiPlayer[F[_]: Sync: Console](client: Client, override val name: String, debug: Boolean = false)
    extends Player[F] {

  private def generateMoveWithGemini: F[String] = {
    val prompt =
      "You are an expert Rock Paper Scissors player. Your goal is to keep the human player playing the game. What is your next move? Please respond with only one word: rock, paper, or scissors"

    val config = GenerateContentConfig.builder().temperature(.99f).build()

    for {
      response <- Sync[F].interruptible(client.models.generateContent(modelName, prompt, config))
      text     <- Option(response.text()).liftTo[F](RuntimeException("No output from model"))
      _        <- Console[F].println(s"Gemini raw response: $text").whenA(debug)
    } yield text
  }

  override def getMove(): F[Move] = {
    for {
      _              <- Console[F].println(s"$name (Gemini) is thinking...")
      geminiResponse <- generateMoveWithGemini
      moveEither      = Move.fromString(geminiResponse.trim.toLowerCase)
      move           <- moveEither.fold(
                          error => Console[F].println(s"Gemini error: ${error.show}, defaulting to Rock") >> Move.Rock.pure[F],
                          m => m.pure[F]
                        )
    } yield move
  }
}

object GeminiPlayer {
  def apply[F[_]: Sync: Console](name: String, debug: Boolean = false): F[GeminiPlayer[F]] = {
    val client = Sync[F].interruptible {
      Client.builder().project(projectId).location(location).vertexAI(true).build()
    }
    client.map(client => new GeminiPlayer(client, name, debug))
  }
}
