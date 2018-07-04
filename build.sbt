import scalariform.formatter.preferences._

name := "quantum-scala"
organization := "com.logicalguess"
version := "1.0-SNAPSHOT"

scalaVersion := "2.12.3"
crossScalaVersions := Seq("2.11.8", "2.12.3")

libraryDependencies ++= Seq(
  "org.apache.commons"        % "commons-math3" % "3.6.1",
  "org.scalacheck" %% "scalacheck" % "1.13.4",
  "org.scalatest" %% "scalatest" % "3.0.1")

scalariformPreferences := scalariformPreferences.value
  .setPreference(DanglingCloseParenthesis, Prevent)
  .setPreference(DoubleIndentClassDeclaration, true)
  .setPreference(PlaceScaladocAsterisksBeneathSecondAsterisk, true)

scalacOptions ++= Seq(
  "-deprecation",
  "-unchecked",
  "-feature",
  "-language:implicitConversions",
  "-language:higherKinds")

licenses := Seq("MIT License" -> url("http://www.opensource.org/licenses/mit-license.php"))

