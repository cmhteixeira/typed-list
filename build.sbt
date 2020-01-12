name := "typed-list"

version := "0.1"

scalaVersion := "2.12.7"

scalacOptions in ThisBuild ++= Seq(
  "-feature", "-language:higherKinds", "-language:existentials", "-explaintypes", "-Xlog-implicits")
