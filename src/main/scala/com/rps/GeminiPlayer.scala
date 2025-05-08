package com.rps

import cats.effect.kernel.Async
import cats.effect.std.Console
import cats.syntax.all.*
import com.google.cloud.vertexai.VertexAI
import com.google.cloud.vertexai.api.{Content, GenerateContentResponse, GenerationConfig, Part, Schema, Type}
import com.google.cloud.vertexai.generativeai.GenerativeModel
import io.circe.generic.auto.*
import io.circe.parser.*

import scala.jdk.CollectionConverters.*

val projectId = sys.env("PROJECT_ID") // Replace with your project ID
val location  = "europe-west1"        // Replace with your location
val modelName = "gemini-2.0-flash"    // Or your preferred model

// Define a case class for the expected JSON structure
case class GeminiResponseData(move: String, comment: String)

class GeminiPlayer[F[_]: Async: Console](val name: String) extends Player[F] {

  private def getVertexAIClient: F[VertexAI] = Async[F].blocking(new VertexAI(projectId, location))

  private def generateMoveWithGemini(client: VertexAI): F[String] = {
    val systemPromptText =
      "You are an expert Rock Paper Scissors player. Your goal is to keep the human playing the game."
    val systemPart       = Part.newBuilder().setText(systemPromptText).build()
    val systemContent    = Content
      .newBuilder()
      .addParts(systemPart)
      .setRole("system")
      .build()

    val moveEnumSchema = Schema
      .newBuilder()
      .setType(Type.STRING)
      .addEnum("Rock")
      .addEnum("Paper")
      .build()

    val commentSchema = Schema.newBuilder().setType(Type.STRING).build()

    val responseSchema = Schema
      .newBuilder()
      .setType(Type.OBJECT)
      .putProperties("move", moveEnumSchema)
      .putProperties("comment", commentSchema)
      .addPropertyOrdering("move")
      .addPropertyOrdering("comment")
      .build()

    val generationConfig = GenerationConfig
      .newBuilder()
      .setResponseMimeType("application/json")
      .setResponseSchema(responseSchema)
      .build()

    val baseGenerativeModel = new GenerativeModel(modelName, client)
      .withSystemInstruction(systemContent) // Apply system instruction here
      .withGenerationConfig(generationConfig)

    for {
      response <- Async[F].blocking {
                    val userPromptText = "What is your next move?"
                    val userPart       = Part.newBuilder().setText(userPromptText).build()
                    val userContent    =
                      com.google.cloud.vertexai.api.Content.newBuilder().setRole("user").addParts(userPart).build()

                    baseGenerativeModel.generateContent(List(userContent).asJava)
                  }
      result   <- response.getCandidatesList.asScala.headOption
                    .liftTo[F](RuntimeException("No output from model"))
                    .flatMap(
                      _.getContent.getPartsList.asScala.headOption
                        .map(_.getText)
                        .liftTo[F](RuntimeException("Response content is empty"))
                    )
      // .flatTap(text => Console[F].println(s"Gemini raw response: $text"))
    } yield result
  }

  override def getMove(): F[Move] = {
    for {
      _                        <- Console[F].println(s"$name (Gemini) is thinking...")
      client                   <- getVertexAIClient
      geminiResponseJsonString <- generateMoveWithGemini(client)
      _                        <- Async[F].blocking(client.close())

      parsedResponseEither = decode[GeminiResponseData](geminiResponseJsonString)

      moveEither <- parsedResponseEither match {
                      case Right(responseData) =>
                        Console[F].println(s"Gemini says: ${responseData.comment}") >>
                          Move.fromString(responseData.move.trim.toLowerCase).pure[F]
                      case Left(parsingError)  =>
                        Console[F].println(
                          s"Gemini error: Failed to parse JSON response: $parsingError. Raw response: $geminiResponseJsonString"
                        ) >> Left(Move.ReadError.InvalidMove("Failed to parse JSON from Gemini")).pure[F]
                    }

      move <- moveEither.fold(
                error =>
                  Console[F].println(s"Gemini move error: ${error.show}, defaulting to Rock") >>
                    Move.Rock.pure[F],
                m => m.pure[F]
              )
    } yield move
  }
}
