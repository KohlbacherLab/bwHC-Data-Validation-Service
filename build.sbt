
/*
 build.sbt adapted from https://github.com/pbassiner/sbt-multi-project-example/blob/master/build.sbt
*/


name := "bwhc-data-validation-service"
organization in ThisBuild := "de.bwhc"
scalaVersion in ThisBuild := "2.13.1"
version := "0.1-SNAPSHOT"


//-----------------------------------------------------------------------------
// PROJECTS
//-----------------------------------------------------------------------------

lazy val global = project
  .in(file("."))
  .settings(
    settings,
    publish / skip := true
  )
  .aggregate(
     api,
     impl
  )


lazy val api = project
  .settings(
    name := "data-validation-service-api",
    settings,
    libraryDependencies ++= Seq(
      dependencies.play_json,
      dependencies.cats_core,
      dependencies.bwhc_utils,
      dependencies.hgnc,
      dependencies.icd_catalogs,
      dependencies.med_catalog
    )
  )


lazy val impl = project
  .settings(
    name := "data-validation-service-impl",
    settings
  )
  .dependsOn(
    api
  )


/*
lazy val fs_repositories = project
  .settings(
    name := "user-service-fs-repos",
    settings,
    libraryDependencies ++= Seq(
      dependencies.repo_utils
    )
  )
  .dependsOn(impl)


lazy val tests = project
  .settings(
    name := "tests",
    settings,
    libraryDependencies ++= Seq(
      dependencies.scalatest,
      dependencies.logback
    ),
    publish / skip := true
  )
  .dependsOn(
    api,
    impl % "test",
    fs_repositories % "test",
  )
*/

//-----------------------------------------------------------------------------
// DEPENDENCIES
//-----------------------------------------------------------------------------

lazy val dependencies =
  new {
    val scalatest    = "org.scalatest"     %% "scalatest"               % "3.1.1" % "test"
    val slf4j        = "org.slf4j"         %  "slf4j-api"               % "1.7.26"
    val logback      = "ch.qos.logback"    %  "logback-classic"         % "1.0.13" % "test"
    val cats_core    = "org.typelevel"     %% "cats-core"               % "2.1.1"
    val play_json    = "com.typesafe.play" %% "play-json"               % "2.8.0"
    val bwhc_utils   = "de.bwhc"           %% "utils"                   % "1.0-SNAPSHOT"
    val hgnc         = "de.bwhc"           %% "hgnc-api"                % "1.0-SNAPSHOT"
    val icd_catalogs = "de.bwhc"           %% "icd-catalogs-api"        % "1.0-SNAPSHOT"
    val med_catalog  = "de.bwhc"           %% "medication-catalog-api"  % "1.0-SNAPSHOT"
//    val repo_utils  = "de.ekut.tbi"       %% "repository-utils"        % "1.0-SNAPSHOT"
  }


//-----------------------------------------------------------------------------
// SETTINGS
//-----------------------------------------------------------------------------

lazy val settings = commonSettings


lazy val compilerOptions = Seq(
  "-encoding", "utf8",
  "-unchecked",
  "-feature",
//  "-language:existentials",
//  "-language:higherKinds",
//  "-language:implicitConversions",
  "-language:postfixOps",
  "-Xfatal-warnings",
  "-deprecation",
)

lazy val commonSettings = Seq(
  scalacOptions ++= compilerOptions,
  resolvers ++= Seq(
    "Local Maven Repository" at "file://" + Path.userHome.absolutePath + "/.m2/repository",
    Resolver.sonatypeRepo("releases"),
    Resolver.sonatypeRepo("snapshots")
  )
)

