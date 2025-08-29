package com.rps

import cats.effect.std.Console
import cats.syntax.all.*
import cats.effect.Ref
import com.google.genai.Client
import com.google.genai.types.GenerateContentConfig
import com.google.genai.types.Schema
import com.google.genai.types.FunctionDeclaration
import com.google.genai.types.Tool
import com.google.genai.types.GenerateContentResponse
import com.google.genai.types.FinishReason
import com.google.genai.types.Content
import com.google.genai.types.Part
import com.google.genai.JsonSerializable
import com.google.common.collect.ImmutableList
import com.google.common.collect.ImmutableMap
import io.circe.*
import io.circe.parser.*
import io.circe.generic.auto.*
import cats.syntax.show.*

import scala.jdk.CollectionConverters.*
import scala.jdk.OptionConverters.*
import cats.effect.kernel.Sync
import cats.ApplicativeThrow
import java.util.{List as JList}
import scala.sys.process.*
import cats.effect.std.Supervisor

val projectId = sys.env("PROJECT_ID")   // Replace with your project ID
val location  = "europe-west1"          // Replace with your location
val modelName = "gemini-2.5-pro" // "gemini-2.0-flash-lite"      // Or your preferred model

// ANSI color constants for console output
private val GREY  = "\u001B[90m"
private val RED   = "\u001B[31m"
private val RESET = "\u001B[0m"
private val BLUE  = "\u001B[34m"
private val PINK  = "\u001B[35m"

// Simplified parameter schemas using basic types
private val emoteParametersSchema = Schema
  .builder()
  .`type`("object")
  .properties(
    ImmutableMap.of(
      "text",
      Schema
        .builder()
        .`type`("string")
        .description("The emote text to display to the opponent")
        .build()
    )
  )
  .required(ImmutableList.of("text"))
  .build()

private val getMovesParametersSchema = Schema.builder().`type`("object").build()

private val getMovesResponseSchema = Schema
  .builder()
  .`type`("object")
  .properties(
    ImmutableMap.of(
      "moves",
      Schema
        .builder()
        .`type`("array")
        .items(Schema.builder().`type`("string").build())
        .description("List of previous moves in chronological order")
        .build()
    )
  )
  .required(ImmutableList.of("moves"))
  .build()

private val moveParametersSchema = Schema
  .builder()
  .`type`("object")
  .properties(
    ImmutableMap.of(
      "move",
      Schema
        .builder()
        .`type`("string")
        .description("The move to make: rock, paper, or scissors")
        .build()
    )
  )
  .required(ImmutableList.of("move"))
  .build()

private val emoteFunction = FunctionDeclaration
  .builder()
  .name("emote")
  .description(
    "Send a message to your opponent"
  )
  .parameters(emoteParametersSchema)
  .build()

private val getMovesFunction = FunctionDeclaration
  .builder()
  .name("get_moves")
  .description("Retrieve the previous moves made in this game session and the number of moves")
  .parameters(getMovesParametersSchema)
  .responseJsonSchema(getMovesResponseSchema)
  .build()

private val moveFunction = FunctionDeclaration
  .builder()
  .name("make_move")
  .description("Make your final move selection for this round of Rock Paper Scissors")
  .parameters(moveParametersSchema)
  .build()

private val functionDeclarations = ImmutableList.of(emoteFunction, getMovesFunction, moveFunction)

private class GeminiPlayer[F[_]: Sync: Console](
    client: Client,
    override val name: String,
    moves: Ref[F, List[RoundResult]],
    opponentName: String,
    debug: Boolean = false
) extends Player[F] {

  private def generateMoveWithGemini: F[Move] = {
    val initialPrompt =
      s"""You are an expert Rock Paper Scissors player. Your goal is to keep the human player playing the game.
      |Your name is $name. Your opponent is $opponentName.
      |You are playing playing a few rounds of Rock Paper Scissors with the human player, whoever wins the most rounds wins the game.
      |
      |Choose your next move (rock, paper, or scissors) and use the available functions to enhance the experience.
      |
      |Change you attitude depending on the previous moves and the result of the previous round: the closer you are to losing, the more you should be mocking your opponent by using emote function. 
      | Loose your temper if you are close to losing.
      |
      |Available functions:
      |1. emote(text) - Display the specified emote text to communicate with your opponent
      |   - text: The exact emote message you want to display 
      |
      |2. get_moves() - Get the list of previous moves made in this game session
      |
      |3. make_move(move) - Make your final move selection for this round
      |   - move: Your move choice ("rock", "paper", or "scissors")
      |
      |When calling emote(), provide the exact text you want displayed as the 'text' parameter.
      |When you are ready to make your move, call make_move() with your chosen move.""".stripMargin

    val initialContent = Content
      .builder()
      .role("system")
      .parts(ImmutableList.of(Part.fromText(initialPrompt)))
      .build()
    conversationLoop(
      initialContent,
      List(
        Content
          .builder()
          .role("user")
          .parts(ImmutableList.of(Part.fromText("I just made my first move, now it's your turn robot")))
          .build()
      ),
      None
    )
  }

  private def conversationLoop(
      systemInstructions: Content,
      history: List[Content],
      foundMove: Option[Move]
  ): F[Move] = {
    val config = GenerateContentConfig
      .builder()
      .systemInstruction(systemInstructions)
      .temperature(2.0f)
      .tools(
        ImmutableList.of(
          Tool
            .builder()
            .functionDeclarations(functionDeclarations)
            .build()
        )
      )
      .build()
    for {
      response <- Sync[F].interruptible(client.models.generateContent(modelName, history.asJava, config))

      result <- if (foundMove.isDefined) {
                  foundMove.get.pure[F]
                } else if (hasFunctionCalls(response)) {
                  for {
                    textResponse           <-
                      response
                        .candidates()
                        .get()
                        .get(0)
                        .content()
                        .get()
                        .parts()
                        .get()
                        .asScala
                        .headOption
                        .flatMap(_.text().asScala)
                        .fold(none.pure[F])(text =>
                          Sync[F].interruptible(s"say -v Rocko -r 200 \"$text\"".!) *> Console[F]
                            .println(s"${PINK}$name (Gemini): ${text.trim()}${RESET}")
                            .as(text.some)
                        )
                    modelPart               = textResponse.map(r => Content.builder().role("model").parts(Part.fromText(r)).build())
                    executionResult        <- executeFunctionCallsAndGetResults(response)
                    (functionResults, move) = executionResult
                    finalMove              <- conversationLoop(systemInstructions, history ++ modelPart ++ functionResults, move)
                  } yield finalMove
                } else {
                  val modelResponse = Option.when(
                    response.candidates().isPresent() && !response.candidates().get().isEmpty()
                  )(response.candidates().get().get(0).content().get())

                  for {
                    moveText      <- Option(response.text()).liftTo[F](RuntimeException("No text response from model"))
                    move          <- extractMoveFromText(moveText)
                    updatedHistory = history ++ modelResponse
                    result        <- conversationLoop(systemInstructions, updatedHistory, move)
                  } yield result
                }
    } yield result
  }

  private def hasFunctionCalls(response: GenerateContentResponse): Boolean = {
    if (response.candidates().isPresent() && !response.candidates().get().isEmpty()) {
      val candidate = response.candidates().get().get(0)
      // Check if any part contains a function call
      candidate.content().isPresent() &&
      candidate.content().get().parts().isPresent() &&
      candidate.content().get().parts().get().asScala.exists(_.functionCall().isPresent())
    } else {
      false
    }
  }

  private def executeFunctionCallsAndGetResults(response: GenerateContentResponse): F[(List[Content], Option[Move])] = {
    if (response.candidates().isPresent() && !response.candidates().get().isEmpty()) {
      val candidate = response.candidates().get().get(0)
      if (candidate.content().isPresent() && candidate.content().get().parts().isPresent()) {
        import scala.jdk.CollectionConverters._

        val parts = candidate.content().get().parts().get().asScala

        val resultOption = parts
          .find(part => part.functionCall().isPresent() && part.functionCall().get().name.isPresent())
          .map(_.functionCall.get())

        val result = resultOption match {
          case Some(functionCall) =>
            val functionCallContent = Content
              .builder()
              .role("model")
              .parts(ImmutableList.of(Part.builder().functionCall(functionCall).build()))
              .build()
            val functionName        = functionCall.name().get()
            functionName match {
              case "emote" =>
                val text = functionCall.args().get().get("text").toString
                Sync[F]
                  .delay {
                    List(
                      functionCallContent,
                      Content
                        .builder()
                        .role("function")
                        .parts(
                          ImmutableList
                            .of(Part.fromFunctionResponse(functionName, ImmutableMap.of("result", "success")))
                        )
                        .build()
                    )
                  }
                  .flatTap(_ => emote(text))
                  .flatTap(_ => Console[F].println(s"${GREY}Model called emote function${RESET}").whenA(debug))
                  .tupleRight(none[Move])

              case "get_moves" =>
                getMoves()
                  .map { moves =>
                    val movesData = moves.map(_.show)
                    List(
                      functionCallContent,
                      Content
                        .builder()
                        .role("function")
                        .parts(
                          ImmutableList
                            .of(Part.fromFunctionResponse(functionName, ImmutableMap.of("moves", movesData.asJava)))
                        )
                        .build()
                    )
                  }
                  .flatTap(c => Console[F].println(s"${GREY}Model called get_moves function: $c ${RESET}").whenA(debug))
                  .tupleRight(none[Move])

              case "make_move" =>
                val moveStr = functionCall.args().get().get("move").toString
                val move    = moveStr match {
                  case "rock"     => Move.Rock
                  case "paper"    => Move.Paper
                  case "scissors" => Move.Scissors
                  case _          => Move.Rock
                }
                Console[F]
                  .println(s"${GREY}Model called make_move function: $moveStr${RESET}")
                  .whenA(debug)
                  .as(
                    (
                      List(
                        functionCallContent,
                        Content
                          .builder()
                          .role("function")
                          .parts(
                            ImmutableList
                              .of(Part.fromFunctionResponse(functionName, ImmutableMap.of("result", "success")))
                          )
                          .build()
                      ),
                      move.some // try changing to none
                    )
                  )

            }
          case None               => (List.empty, none[Move]).pure[F]
        }

        result
      } else {
        (List.empty[Content], none).pure[F]
      }
    } else {
      (List.empty[Content], none).pure[F]
    }
  }

  private def extractMoveFromText(text: String): F[Option[Move]] = {
    // Look for move in text (case insensitive)
    val lowerText = text.toLowerCase
    val move =
      if (lowerText.contains("rock")) Move.Rock.some
      else if (lowerText.contains("paper")) Move.Paper.some
      else if (lowerText.contains("scissors")) Move.Scissors.some
      else None

    Console[F].println(s"${RED}Extracted move from text '$text': $move${RESET}").whenA(debug) >>
      move.pure[F]
  }

  private def emote(text: String): F[Unit]     = Console[F].println(s"${PINK}$name (Gemini): $text${RESET}")
  private def getMoves(): F[List[RoundResult]] = moves.get.map(_.reverse)

  override def getMove(): F[Move] = {
    for {
      _    <- Console[F].println(s"${GREY}$name (Gemini) is thinking...${RESET}")
      move <- generateMoveWithGemini
      _    <- Console[F].println(s"${BLUE}$name (Gemini) picks $move${RESET}")
    } yield move
  }
}

object GeminiPlayer {
  def apply[F[_]: Sync: Console](
      name: String,
      debug: Boolean = false,
      movesRef: Ref[F, List[RoundResult]],
      opponentName: String
  ): F[GeminiPlayer[F]] = {
    for {
      _      <-
        Console[F].println(
          s"LEGEND: ${GREY} GREY is debug logging ${RESET} ${BLUE}BLUE is model response ${RESET} ${RED}RED is move extraction ${RESET} ${PINK}PINK is model responses ${RESET}"
        )
      client <- Sync[F].interruptible {
                  Client.builder().project(projectId).location(location).vertexAI(true).build()
                }
    } yield new GeminiPlayer(client, name, movesRef, opponentName, debug)
  }
}
