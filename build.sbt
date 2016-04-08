name := "play-scala"

version := "1.0-SNAPSHOT"

lazy val root = (project in file(".")).enablePlugins(PlayScala)

scalaVersion := "2.11.7"

libraryDependencies ++= Seq(
  jdbc,
  cache,
  ws,
  "org.scalatestplus.play" %% "scalatestplus-play" % "1.5.0-RC1" % Test,
  "io.reactivex" %% "rxscala" % "0.26.0"

)

resolvers += "scalaz-bintray" at "http://dl.bintray.com/scalaz/releases"
