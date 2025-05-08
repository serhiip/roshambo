package com.rps

import cats.effect.kernel.Async
import cats.effect.std.Console
import cats.syntax.all.*
import com.google.cloud.vertexai.VertexAI
import com.google.cloud.vertexai.api.{Candidate, Content, FunctionDeclaration, GenerateContentResponse, GenerationConfig, Part, Schema, Tool, Type, ToolConfig, FunctionCallingConfig}
import com.google.cloud.vertexai.generativeai.{GenerativeModel, ResponseHandler}
import io.circe.generic.auto.*
import io.circe.parser.*

import scala.jdk.CollectionConverters.*
import com.google.protobuf.util.JsonFormat

val projectId = sys.env("PROJECT_ID") // Replace with your project ID
val location  = "europe-west1"        // Replace with your location
val modelName = "gemini-2.0-flash"    // Or your preferred model

// Define a case class for the expected JSON structure
case class GeminiResponseData(move: String, comment: String)

class GeminiPlayer[F[_]: Async: Console](val name: String) extends Player[F] {

  private def getVertexAIClient: F[VertexAI] = Async[F].blocking(new VertexAI(projectId, location))

  private def generateMoveWithGemini(client: VertexAI, currentGameHistory: List[Result]): F[String] = {
    val systemPromptText =
      "You are an expert Rock Paper Scissors player. Your goal is to keep the human playing the game. " +
      "You can call the 'getPreviousRounds' function if you need to see the history of previous rounds to inform your decision. " +
      "IMPORTANT: After you receive the response from the 'getPreviousRounds' function, you MUST use that information to decide your next move and then provide your final response. " +
      "Your final response MUST be a JSON object with 'move' (one of 'Rock', 'Paper', 'Scissors') and 'comment' keys ONLY."
    val systemPart       = Part.newBuilder().setText(systemPromptText).build()
    val systemInstructionContent = Content.newBuilder().addParts(systemPart).build()

    val getPreviousRoundsResponseSchema = Schema.newBuilder()
      .setType(Type.OBJECT)
      .putProperties("history", Schema.newBuilder().setType(Type.STRING).setDescription("A summary of previous rounds, each on a new line.").build())
      .addRequired("history")
      .build()

    val getPreviousRoundsFn = FunctionDeclaration.newBuilder()
      .setName("getPreviousRounds")
      .setDescription("Gets the history of previous rounds in the current game, including who won each round or if it was a tie. Call this if you need context from past rounds.")
      .setResponse(getPreviousRoundsResponseSchema)
      .build()

    val gameTool = Tool.newBuilder().addFunctionDeclarations(getPreviousRoundsFn).build()

    val functionCallingConfig = FunctionCallingConfig.newBuilder()
      .setMode(FunctionCallingConfig.Mode.ANY)
      .build()

    val toolConfig = ToolConfig.newBuilder()
      .setFunctionCallingConfig(functionCallingConfig)
      .build()
      
    val generationConfig = GenerationConfig.newBuilder()
      .build()

    val model = new GenerativeModel(modelName, client)
      .withSystemInstruction(systemInstructionContent)
      .withGenerationConfig(generationConfig)
      .withTools(List(gameTool).asJava)
      .withToolConfig(toolConfig)

    val historySummaryStr = 
      if (currentGameHistory.isEmpty) "No rounds played yet in this game."
      else currentGameHistory.zipWithIndex.map { case (res, idx) =>
        s"Round ${idx + 1}: ${res.show}"
      }.mkString(". ")

    val userPromptText = s"Previous rounds summary: $historySummaryStr. What is your next move?"
    val initialUserContent = Content.newBuilder().setRole("user").addParts(Part.newBuilder().setText(userPromptText)).build()

    def processResponse(history: List[Content], turnsLeft: Int): F[String] = {
      if (turnsLeft <= 0) {
        Async[F].raiseError(new RuntimeException("Exceeded maximum function call turns."))
      } else {
        Async[F].blocking { model.generateContent(history.asJava) }.flatMap { response =>
          val functionCalls = ResponseHandler.getFunctionCalls(response).asScala
          println(functionCalls)
          if (functionCalls.nonEmpty && functionCalls.exists(_.getName == "getPreviousRounds")) {
             Console[F].println(s"$name (Gemini) is requesting previous rounds history.") >>
             Async[F].blocking {
                val actualHistoryStr =
                  if (currentGameHistory.isEmpty) "No rounds played yet in this game."
                  else currentGameHistory.zipWithIndex.map { case (res, idx) =>
                    s"Round ${idx + 1}: ${res.show}"
                  }.mkString("\n")

                val responseFields = com.google.protobuf.Struct.newBuilder()
                  .putFields("history", com.google.protobuf.Value.newBuilder().setStringValue(actualHistoryStr).build())
                  .build()

                val fnResponseProto = com.google.cloud.vertexai.api.FunctionResponse.newBuilder()
                  .setName("getPreviousRounds")
                  .setResponse(responseFields)
                  .build()

                val functionResponseContent = Content.newBuilder()
                  .addParts(Part.newBuilder().setFunctionResponse(fnResponseProto))
                  .setRole("function")
                  .build()

                val modelFunctionCallContent = response.getCandidatesList.asScala.head.getContent

                val nextHistory = functionResponseContent :: modelFunctionCallContent :: history
                
                (nextHistory, turnsLeft - 1)
             }.flatMap { case (nextHistory, remainingTurns) => 
                processResponse(nextHistory, remainingTurns)
             }
          } else {
             Async[F].pure(ResponseHandler.getText(response))
          }
        }
      }
    }

    processResponse(List(initialUserContent), 2)
  }

  override def getMove(currentGameHistory: List[Result]): F[Move] = {
    for {
      _                        <- Console[F].println(s"$name (Gemini) is thinking...")
      client                   <- getVertexAIClient
      geminiResponseJsonString <- generateMoveWithGemini(client, currentGameHistory)
      _                        <- Async[F].blocking(client.close())

      _ <- Console[F].println(s"Gemini raw response: $geminiResponseJsonString")

      parsedResponseEither = decode[GeminiResponseData](geminiResponseJsonString)

      moveEither <- parsedResponseEither match {
                      case Right(responseData) =>
                        Console[F].println(s"$name says: ${responseData.comment}") >>
                          Move.fromString(responseData.move.trim.toLowerCase).pure[F]
                      case Left(parsingError)  =>
                        Console[F].println(
                          s"$name error: Failed to parse JSON response: $parsingError. Raw response: $geminiResponseJsonString"
                        ) >> Left(Move.ReadError.InvalidMove("Failed to parse JSON from Gemini")).pure[F]
                    }

      move <- moveEither.fold(
                error =>
                  Console[F].println(s"$name move error: ${error.show}, defaulting to Rock") >>
                    Move.Rock.pure[F],
                m => m.pure[F]
              )
    } yield move
  }
}
