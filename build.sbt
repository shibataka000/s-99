lazy val root = (project in file(".")).
  settings(
    name := "s-99",
    version := "1.0",
    scalaVersion := "2.11.7"
  )

libraryDependencies ++= Seq("org.specs2" %% "specs2-core" % "3.6.3" % "test")

resolvers += "scalaz-bintray" at "http://dl.bintray.com/scalaz/releases"

scalacOptions in Test ++= Seq("-Yrangepos")
