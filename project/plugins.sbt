// ⚠️ Remember to run `sbt bloopInstall` after modifying this file
addSbtPlugin("com.github.sbt"     % "sbt-native-packager"      % "1.11.1")
addSbtPlugin("org.scala-js"       % "sbt-scalajs"              % "1.21.0")
addSbtPlugin("org.portable-scala" % "sbt-scalajs-crossproject" % "1.3.2")
addSbtPlugin("ch.epfl.scala"      % "sbt-bloop"                % "2.0.8")

libraryDependencies += "org.scala-js" %% "scalajs-env-jsdom-nodejs" % "1.1.0"
