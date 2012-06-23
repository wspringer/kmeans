libraryDependencies ++= Seq(
  "org.specs2" %% "specs2" % "1.5" % "test" excludeAll(
    ExclusionRule(organization = "org.scala-lang")
  ),
  "org.mockito" % "mockito-all" % "1.8.4" % "test"
)