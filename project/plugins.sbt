addSbtPlugin("com.geirsson" % "sbt-ci-release" % "1.2.6")
addSbtPlugin("org.scala-js" % "sbt-scalajs" % "0.6.28")
addSbtPlugin("org.portable-scala" % "sbt-scalajs-crossproject" % "0.6.1")
addSbtPlugin("com.jsuereth" % "sbt-pgp" % "1.1.2")
addSbtPlugin("com.typesafe" % "sbt-mima-plugin" % "0.5.0")
addSbtPlugin("io.get-coursier" % "sbt-shading" % sbtCoursierVersion)

addSbtCoursier

val circeVersion = "0.11.0"

lazy val root = (project in file(".")) dependsOn codegen

lazy val codegen = (project in file ("codegen"))
  .settings(
    libraryDependencies ++= Seq(
      "io.circe" %% "circe-core",
      "io.circe" %% "circe-generic-extras",
      "io.circe" %% "circe-parser",
      "io.circe" %% "circe-optics",
    ).map(_ % circeVersion),
    libraryDependencies ++= Seq(
      "com.lihaoyi" %% "os-lib" % "0.2.8",
      "org.scalameta" %% "scalameta" % "4.2.2",
      "org.scalameta" %% "scalafmt-core" % "2.0.0-RC5",
    )
  )