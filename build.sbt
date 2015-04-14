import com.lihaoyi.workbench.Plugin._

/**
 * Standard Scala.js settings
 */

enablePlugins(ScalaJSPlugin)

workbenchSettings

name := "PhasePortraitJS"

version := "1.0"

scalaVersion := "2.11.6"


libraryDependencies ++= Seq(
  "org.scala-js" %%% "scalajs-dom" % "0.8.0"
)

bootSnippet := "drawing.ScalaJSPhasePortrait().main(document.getElementById('canvas'));"

updateBrowsers <<= updateBrowsers.triggeredBy(fastOptJS in Compile)

