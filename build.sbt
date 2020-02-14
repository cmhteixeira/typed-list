name := "typed-list"

version := "1.0.0"

scalaVersion := "2.12.7"

scalacOptions in ThisBuild ++= Seq(
  "-feature", "-language:higherKinds", "-language:existentials", "-explaintypes", "-Xlog-implicits")
