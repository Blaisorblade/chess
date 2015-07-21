//organization := "chess"

name := "chess"

version := "0.1-SNAPSHOT"

scalaVersion := "2.11.6"

scalaSource in Compile := baseDirectory.value / "src"
scalaSource in Test := baseDirectory.value / "test-src"

//Just to stop sbteclipse from creating extra dirs.
javaSource in Compile := baseDirectory.value / "src"
javaSource in Test := baseDirectory.value / "test-src"
