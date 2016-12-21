lazy val theVersion = "0.1.0-SNAPSHOT"

// scala.meta macros are at the moment only supported in 2.11.
lazy val theScalaVersion = "2.11.8"

lazy val root = Project(id = "freast-root", base = file("."))
  .settings(
    name := "freast-root",
    crossVersion := CrossVersion.binary,
    publishSettings,
    // Do not publish the root project (it just serves as an aggregate)
    publishArtifact := false,
    publishLocal := {}
  )
  .aggregate(core, cats, scalaz)

lazy val core = project.settings(
  name := "freast-core",
  commonSettings,
  metaMacroSettings,
  publishSettings,
  // A dependency on scala.meta is required to write new-style macros, but not
  // to expand such macros.  This is similar to how it works for old-style
  // macros and a dependency on scala.reflect.
  libraryDependencies ++= commonDependencies
)

lazy val cats = project
  .settings(
    name := "freast-cats",
    commonSettings,
    metaMacroSettings,
    publishSettings,
    libraryDependencies ++= {
      commonDependencies :+ "org.typelevel" %% "cats" % "0.8.1"
    }
  )
  .dependsOn(core)

lazy val scalaz = project
  .settings(
    name := "freast-scalaz",
    commonSettings,
    metaMacroSettings,
    publishSettings,
    libraryDependencies ++= {
      commonDependencies :+ "org.scalaz"    %% "scalaz-core" % "7.2.8"
    }
  )
  .dependsOn(core)

lazy val commonSettings: Seq[Def.Setting[_]] = Seq(
  organization := "com.beachape",
  version := theVersion,
  scalaVersion := theScalaVersion
)

lazy val metaMacroSettings: Seq[Def.Setting[_]] = Seq(
  // New-style macro annotations are under active development.  As a result, in
  // this build we'll be referring to snapshot versions of both scala.meta and
  // macro paradise.
  resolvers += Resolver.url("scalameta", url("http://dl.bintray.com/scalameta/maven"))(
    Resolver.ivyStylePatterns),
  // A dependency on macro paradise is required to both write and expand
  // new-style macros.  This is similar to how it works for old-style macro
  // annotations and a dependency on macro paradise 2.x.
  addCompilerPlugin("org.scalameta" % "paradise" % "4.0.0.142" cross CrossVersion.full),
  scalacOptions += "-Xplugin-require:macroparadise",
  // temporary workaround for https://github.com/scalameta/paradise/issues/10
  scalacOptions in (Compile, console) := Seq(), // macroparadise plugin doesn't work in repl yet.
  // temporary workaround for https://github.com/scalameta/paradise/issues/55
  sources in (Compile, doc) := Nil // macroparadise doesn't work with scaladoc yet.
)

lazy val commonDependencies = Seq(
  "org.scalatest" %% "scalatest"   % "3.0.1-SNAP1" % Test,
  "org.scalameta" %% "scalameta"   % "1.4.0"
)

// Settings for publishing to Maven Central
lazy val publishSettings: Seq[Def.Setting[_]] = Seq(
  pomExtra :=
    <url>https://github.com/lloydmeta/freast</url>
      <licenses>
        <license>
          <name>MIT</name>
          <url>http://opensource.org/licenses/MIT</url>
          <distribution>repo</distribution>
        </license>
      </licenses>
      <scm>
        <url>git@github.com:lloydmeta/freast.git</url>
        <connection>scm:git:git@github.com:lloydmeta/freast.git</connection>
      </scm>
      <developers>
        <developer>
          <id>lloydmeta</id>
          <name>Lloyd Chan</name>
          <url>https://beachape.com</url>
        </developer>
      </developers>,
  publishTo := {
    val nexus = "https://oss.sonatype.org/"
    if (version.value.trim.endsWith("SNAPSHOT"))
      Some("snapshots" at nexus + "content/repositories/snapshots")
    else
      Some("releases" at nexus + "service/local/staging/deploy/maven2")
  },
  publishMavenStyle := true,
  publishArtifact in Test := false,
  pomIncludeRepository := { _ =>
    false
  }
)

scalacOptions ++= Seq(
  "-deprecation",
  "-encoding",
  "UTF-8",
  "-feature",
  "-language:existentials",
  "-language:higherKinds",
  "-language:implicitConversions",
  "-unchecked",
  "-Xfatal-warnings",
  "-Xlint",
  "-Yno-adapted-args",
  "-Ywarn-dead-code",
  "-Ywarn-numeric-widen",
  "-Ywarn-value-discard",
  "-Xfuture",
  "-Ywarn-unused-import"
)