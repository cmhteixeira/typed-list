name := "typed-list"

version := "0.1"

scalaVersion := "2.12.7"

scalacOptions in ThisBuild ++= Seq(
  "-feature", "-language:higherKinds", "-language:existentials", "-explaintypes", "-Xlog-implicits")


initialCommands := """
  import com.github.cmhteixeira._
  import com.github.cmhteixeira.typedlist._
  import com.github.cmhteixeira.typedlist.TypedList
  import com.github.cmhteixeira.naturalnumbers.Natural._
  import com.github.cmhteixeira.naturalnumbers.LowerOrEqual
  """
