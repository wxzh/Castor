scalaVersion in ThisBuild := "2.12.3"

lazy val localScalacOpts = Seq(
  "-language:higherKinds",
  "-language:implicitConversions",
  "-language:reflectiveCalls",
  "-feature",
  "-deprecation")

lazy val metaMacroSettings: Seq[Def.Setting[_]] = Seq(
  resolvers += Resolver.sonatypeRepo("releases"),
  resolvers += Resolver.bintrayRepo("scalameta", "maven"),

  addCompilerPlugin("org.scalameta" % "paradise" % "3.0.0-M10" cross CrossVersion.full),

  scalacOptions += "-Xplugin-require:macroparadise",
  scalacOptions ++= localScalacOpts,
  scalacOptions in(Compile, console) := localScalacOpts
)

lazy val macros = project.settings(
  metaMacroSettings,
  libraryDependencies += "org.scalameta" %% "scalameta" % "1.8.0" //% Provided
)

lazy val tapl = project.settings(
  metaMacroSettings,
  scalacOptions ++= localScalacOpts,
  addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.4"),
  libraryDependencies ++= Seq(
    "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.6",
    "com.storm-enroute" %% "scalameter" % "0.8.2" % "test",
    "org.scalatest" %% "scalatest" % "3.0.3" % "test"
  ),
  testFrameworks += new TestFramework("org.scalameter.ScalaMeterFramework"),
  parallelExecution in Test := false,
  logBuffered := false
).dependsOn(macros)

