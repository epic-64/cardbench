import org.scalajs.jsenv.jsdomnodejs.JSDOMNodeJSEnv
import org.portablescala.sbtplatformdeps.PlatformDepsPlugin.autoImport.*
import sbtcrossproject.CrossPlugin.autoImport.*

ThisBuild / version      := "0.1.0-SNAPSHOT"
ThisBuild / scalaVersion := "3.8.1"
ThisBuild / scalacOptions ++= Seq("-Werror", "-deprecation")

val caskVersion = "0.11.3"

// ⚠️ Remember to run `sbt bloopInstall` after modifying this file

// Root project that aggregates the JS and JVM subprojects.
lazy val root = project
  .in(file("."))
  .aggregate(app.js, app.jvm)
  .settings(
    name := "gigadev",
    publish / skip := true,
  )

// Cross-project: shared code plus platform-specific code.
lazy val app = crossProject(JSPlatform, JVMPlatform)
  .in(file("."))
  .jsConfigure(_.withId("js"))
  .jvmConfigure(_.withId("jvm"))
  .settings(
    name := "gigadev",
    // Quiet ScalaTest reporter; failures + final summary still print.
    Test / testOptions += Tests.Argument(TestFrameworks.ScalaTest, "-oNCXEHLOPQRM"),
    libraryDependencies ++= Seq(
      "com.lihaoyi"   %%% "upickle"   % "4.0.2",
      "org.scalatest" %%% "scalatest" % "3.2.19" % Test,
    ),
  )
  .jvmSettings(
    Compile / mainClass  := Some("server.WebServer"),
    executableScriptName := "main",
    libraryDependencies ++= Seq(
      "com.lihaoyi"    %% "cask"            % caskVersion,
      "ch.qos.logback"  % "logback-classic" % "1.5.34",
    ),
  )
  .jvmEnablePlugins(JavaAppPackaging)
  .jsSettings(
    // Generate main.js with a module initializer so `js/run` works.
    scalaJSUseMainModuleInitializer := true,
    // jsdom provides a DOM for tests.
    Test / jsEnv := new JSDOMNodeJSEnv(),
    // Emit linked JS straight into the JVM static dir for serving.
    Compile / fastLinkJS / scalaJSLinkerOutputDirectory :=
      baseDirectory.value / ".." / "jvm" / "src" / "main" / "resources" / "static" / "dist",
    Compile / fullLinkJS / scalaJSLinkerOutputDirectory :=
      baseDirectory.value / ".." / "jvm" / "src" / "main" / "resources" / "static" / "dist",
    libraryDependencies ++= Seq(
      "org.scala-js" %%% "scalajs-dom" % "2.2.0",
      "com.raquo"    %%% "laminar"     % "17.2.0",
    ),
  )
