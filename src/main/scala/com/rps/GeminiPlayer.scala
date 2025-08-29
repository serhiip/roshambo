package com.rps

import cats.effect.std.Console
import cats.syntax.all.*
import com.google.genai.Client
import com.google.genai.types.GenerateContentConfig
import com.google.genai.types.Schema
import com.google.common.collect.ImmutableList
import com.google.common.collect.ImmutableMap
import io.circe.*
import io.circe.parser.*
import io.circe.generic.auto.*
import cats.syntax.show.*

import scala.jdk.CollectionConverters.*
import cats.effect.kernel.Sync
import cats.ApplicativeThrow

val projectId = sys.env("PROJECT_ID") // Replace with your project ID
val location  = "europe-west1"        // Replace with your location
val modelName = "gemini-2.0-flash"    // Or your preferred model

private case class MoveResponse(move: String, emote: String)

// ANSI color constants for console output
private val GREY  = "\u001B[90m"
private val RED   = "\u001B[31m"
private val RESET = "\u001B[0m"
private val BLUE  = "\u001B[34m"
private val PINK  = "\u001B[35m"

private val moveSchema: Schema  = Schema.builder().`type`("string").build()
private val emoteSchema: Schema = Schema.builder().`type`("string").build()

private val responseSchema: Schema = Schema
  .builder()
  .`type`("object")
  .properties(ImmutableMap.of("move", moveSchema, "emote", emoteSchema))
  .required(ImmutableList.of("move", "emote"))
  .build()

private class GeminiPlayer[F[_]: Sync: Console](client: Client, override val name: String, debug: Boolean = false)
    extends Player[F] {

  private def generateMoveWithGemini: F[(Move, String)] = {
    val prompt =
      """You are an expert Rock Paper Scissors player. Your goal is to keep the human player playing the game.

      |Choose your next move and create a fun emote/expression to encourage the player to continue playing.
      |
      |Respond with a JSON object containing both the "move" field and an "emote" field.""".stripMargin

    val config = GenerateContentConfig
      .builder()
      .temperature(.99f)
      .responseMimeType("application/json")
      .responseSchema(responseSchema)
      .build()

    for {
      response     <- Sync[F].interruptible(client.models.generateContent(modelName, prompt, config))
      text         <- Option(response.text()).liftTo[F](RuntimeException("No output from model"))
      _            <- Console[F].println(s"${RED}Gemini raw response: $text${RESET}").whenA(debug)
      moveResponse <- decode[MoveResponse](text).leftMap(e => RuntimeException("Failed to decode JSON", e)).liftTo[F]
      move         <- Move
                        .fromString(moveResponse.move.toLowerCase)
                        .leftMap(e => RuntimeException(show"Failed to parse move: $e"))
                        .liftTo[F]
    } yield (move, moveResponse.emote)
  }

  override def getMove(): F[Move] = {
    for {
      _             <- Console[F].println(s"${GREY}$name (Gemini) is thinking...${RESET}")
      (move, emote) <- generateMoveWithGemini
      _             <- Console[F].println(s"${BLUE}$name (Gemini) chose $move${RESET}")
      _             <- Console[F].println(s"${PINK}$emote${RESET}")
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
