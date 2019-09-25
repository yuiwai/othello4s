import sbtcrossproject.CrossPlugin.autoImport.{crossProject, CrossType}

version in ThisBuild := "0.1"
scalaVersion in ThisBuild := "2.12.8"
val circeVersion = "0.11.1"

lazy val core =
  crossProject(JSPlatform, JVMPlatform)
    .crossType(CrossType.Pure)
    .in(file("core"))
    .settings(
      name := "othello-core",
      testFrameworks += new TestFramework("utest.runner.Framework")
    )
    .jvmSettings(
      libraryDependencies ++= Seq(
        "com.lihaoyi" %% "utest" % "0.7.1" % "test"
      )
    )

lazy val coreJVM = core.jvm
lazy val coreJS = core.js

lazy val service =
  crossProject(JVMPlatform, JSPlatform)
    .crossType(CrossType.Pure)
    .settings(
      name := "othello-service"
    )
    .dependsOn(core)

lazy val serviceJVM = service.jvm
lazy val serviceJS = service.js

lazy val circeCodec =
  crossProject(JVMPlatform, JSPlatform)
    .crossType(CrossType.Pure)
    .in(file("codec/circe"))
    .settings(
      name := "othello-codec-circe",
      libraryDependencies ++= Seq(
        "io.circe" %% "circe-core",
        "io.circe" %% "circe-generic",
        "io.circe" %% "circe-parser"
      ).map(_ % circeVersion),
    )
    .jsSettings(
      libraryDependencies ++= Seq(
        "io.circe" %%% "circe-core",
        "io.circe" %%% "circe-generic",
        "io.circe" %%% "circe-parser"
      ).map(_ % circeVersion),
    )
    .dependsOn(service)

lazy val circeCodecJVM = circeCodec.jvm
lazy val circeCodecJS = circeCodec.js


lazy val upickleCodec =
  crossProject(JVMPlatform, JSPlatform)
    .crossType(CrossType.Pure)
    .in(file("codec/upickle"))
    .settings(
      name := "othello-codec-upickle",
      libraryDependencies ++= Seq()
    )
    .dependsOn(service)

lazy val upickleCodecJVM = upickleCodec.jvm
lazy val upickleCodecJS = upickleCodec.js

lazy val akkaServer =
  project
    .in(file("server/akka"))
    .settings(
      name := "othello-server-akka",
      libraryDependencies ++= Seq(
        "com.typesafe.akka" %% "akka-http" % "10.1.9",
        "com.typesafe.akka" %% "akka-stream" % "2.5.23",
        "de.heikoseeberger" %% "akka-http-circe" % "1.27.0",
      ),
      scalaJSProjects := Seq(reactClient),
      pipelineStages in Assets := Seq(scalaJSPipeline),
      WebKeys.packagePrefix in Assets := "public/",
      managedClasspath in Runtime += (packageBin in Assets).value,
      compile in Compile := ((compile in Compile) dependsOn scalaJSPipeline).value,

      dockerExposedPorts ++= Seq(8080)
    )
    .dependsOn(serviceJVM, circeCodecJVM)
    .enablePlugins(WebScalaJSBundlerPlugin, JavaServerAppPackaging, DockerPlugin)

lazy val reactClient =
  project
    .in(file("client/react"))
    .settings(
      name := "othello-client-react",
      scalaJSUseMainModuleInitializer := true,
      libraryDependencies ++= Seq(
        "com.github.japgolly.scalajs-react" %%% "core" % "1.4.2",
        "org.scala-js" %%% "scalajs-dom" % "0.9.7",
        "com.github.japgolly.scalacss" %%% "core" % "0.5.3",
        "com.github.japgolly.scalacss" %%% "ext-react" % "0.5.3"
      ),
      npmDependencies in Compile ++= Seq(
        "react" -> "16.7.0",
        "react-dom" -> "16.7.0"
      ),
    )
    .dependsOn(serviceJS, circeCodecJS)
    .enablePlugins(ScalaJSBundlerPlugin)

lazy val check = project
  .in(file("check"))
  .settings(
    name := "othello-check"
  )
  .dependsOn(circeCodecJVM, coreJVM)
