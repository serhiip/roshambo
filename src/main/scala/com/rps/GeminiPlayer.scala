package com.rps

import cats.effect.kernel.Async
import cats.effect.std.Console
import cats.syntax.all.*
import com.google.cloud.vertexai.VertexAI
import com.google.cloud.vertexai.api.{GenerateContentResponse, Part}
import com.google.cloud.vertexai.generativeai.GenerativeModel

import scala.jdk.CollectionConverters.*

val projectId = sys.env("PROJECT_ID") // Replace with your project ID
val location  = "europe-west1"        // Replace with your location
val modelName = "gemini-2.0-flash"    // Or your preferred model

class GeminiPlayer[F[_]: Async: Console](val name: String) extends Player[F] {

  private def getVertexAIClient: F[VertexAI] = Async[F].blocking(new VertexAI(projectId, location))

  private def generateMoveWithGemini(client: VertexAI): F[String] = {
    val systemPromptText =
      "You are an expert Rock Paper Scissors player. Your goal is to keep the human player playing the game"
    val systemPart       = Part.newBuilder().setText(systemPromptText).build()
    val systemContent    = com.google.cloud.vertexai.api.Content.newBuilder().addParts(systemPart).build()

    val baseGenerativeModel             = new GenerativeModel(modelName, client)
    val generativeModelWithSystemPrompt = baseGenerativeModel.withSystemInstruction(systemContent)

    for {
      response <- Async[F].blocking {
                    val userPromptText = "What is your next move?"
                    val userPart       = Part.newBuilder().setText(userPromptText).build()
                    val userContent    =
                      com.google.cloud.vertexai.api.Content.newBuilder().setRole("user").addParts(userPart).build()

                    generativeModelWithSystemPrompt.generateContent(List(userContent).asJava)
                  }
      result   <- response.getCandidatesList.asScala.headOption
                    .liftTo[F](RuntimeException("No output from model"))
                    .flatMap(
                      _.getContent.getPartsList.asScala.headOption
                        .map(_.getText)
                        .liftTo[F](RuntimeException("Response content is empty"))
                    )
//                    .flatTap(text => Console[F].println(s"Gemini raw response: $text"))
    } yield result
  }

  override def getMove(): F[Move] = {
    for {
      _              <- Console[F].println(s"$name (Gemini) is thinking...")
      client         <- getVertexAIClient
      geminiResponse <- generateMoveWithGemini(client)
      _              <- Async[F].blocking(client.close())
      moveEither      = Move.fromString(geminiResponse.trim.toLowerCase)
      move           <- moveEither.fold(
                          error => Console[F].println(s"Gemini error: ${error.show}, defaulting to Rock") >> Move.Rock.pure[F],
                          m => m.pure[F]
                        )
    } yield move
  }
}
