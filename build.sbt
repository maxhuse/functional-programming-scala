name := "functional-programming-scala"

version := "1.0"

scalaVersion := "2.13.1"

//libraryDependencies += "com.novocode" % "junit-interface" % "0.11" % Test
//libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.14.0"


libraryDependencies += "org.scalatest" %% "scalatest" % "3.1.1" % "test"
libraryDependencies += "junit" % "junit" % "4.10" % "test"

testOptions in Test += Tests.Argument(TestFrameworks.JUnit, "-a", "-v", "-s")