import com.waioeka.sbt.CucumberPlugin
import sbt.Keys._
import sbt.{Build => SbtBuild, _}
import sbtassembly.Plugin.AssemblyKeys._
import sbtassembly.Plugin._

object Build extends SbtBuild {

  val Organization = "gray"
  val Name = "markdown-rendering"
  val Version = Option(System.getenv("BUILD_VERSION")) getOrElse "DEV"
  val ScalaVersion = "2.11.8"
  val ScalatraVersion = "2.4.1"

  val dependencies = Seq(
    "org.scalatest" %% "scalatest" % "3.0.0" % "test",
    "org.scalatra" %% "scalatra-scalatest" % ScalatraVersion % "test",
    "info.cukes" %% "cucumber-scala" % "1.2.4" % "test"
  )

  lazy val project = Project(
    Name,
    file("."),
    settings =
      Defaults.coreDefaultSettings ++
        assemblySettings ++
        Seq(
          unmanagedResourceDirectories in Compile <+= baseDirectory(_ / "test/resources"),
          unmanagedResourceDirectories in Compile <+= baseDirectory(_ / "test/resources/*"),
          CucumberPlugin.glue := "cucumber/steps/",
          CucumberPlugin.features := List("cucumber")
        ) ++
        Seq(scalacOptions ++= Seq("-feature", "-target:jvm-1.7", "-language:postfixOps")) ++
        Seq(
          organization := Organization,
          name := Name,
          version := Version,
          scalaVersion := ScalaVersion,
          resolvers += Classpaths.typesafeReleases,
          jarName in assembly := s"$Name.jar",
          mainClass in assembly := Some("com.gray.markdown.Main"),
          mergeStrategy in assembly := {
            case PathList("mime.types") => MergeStrategy.first
            case x =>
              val oldStrategy = (mergeStrategy in assembly).value
              oldStrategy(x)
          },
          libraryDependencies ++= dependencies
        )
  ).enablePlugins(CucumberPlugin)

}
