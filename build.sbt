ThisBuild / organization := "com.example"
ThisBuild / scalaVersion := "3.6.4"

lazy val root = (project in file(".")).settings(
  name := "rockpaperscissors",
  libraryDependencies ++= Seq(
    // "core" module - IO, IOApp, schedulers
    // This pulls in the kernel and std modules automatically.
    "org.typelevel"   %% "cats-effect"                % "3.5.4",
    // concurrency abstractions and primitives (Concurrent, Sync, Async etc.)
    "org.typelevel"   %% "cats-effect-kernel"         % "3.3.12",
    // standard "effect" library (Queues, Console, Random etc.)
    "org.typelevel"   %% "cats-effect-std"            % "3.5.4",
    "org.typelevel"   %% "cats-effect-testing-specs2" % "1.4.0" % Test,
    "com.google.genai" % "google-genai"               % "1.15.0",
    // Guava for Immutable collections (used by Google Gen AI SDK)
    "com.google.guava" % "guava"                      % "33.0.0-jre",
    // JSON parsing
    "io.circe"        %% "circe-core"                 % "0.14.6",
    "io.circe"        %% "circe-parser"               % "0.14.6",
    "io.circe"        %% "circe-generic"              % "0.14.6"
  )
)
