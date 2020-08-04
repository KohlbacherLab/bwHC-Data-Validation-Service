
/*
 build.sbt adapted from https://github.com/pbassiner/sbt-multi-project-example/blob/master/build.sbt
*/


name := "bwhc-data-entry-service"
organization in ThisBuild := "de.bwhc"
scalaVersion in ThisBuild := "2.13.1"
version in ThisBuild := "1.0-SNAPSHOT"


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
     generators,
     impl,
     deps,
     tests
  )


lazy val api = project
  .settings(
    name := "data-entry-service-api",
    settings,
    libraryDependencies ++= Seq(
      dependencies.play_json,
      dependencies.cats_core,
      dependencies.bwhc_utils,
    )
  )


lazy val generators = project
  .settings(
    name := "mtb-dto-generators",
    settings,
    libraryDependencies ++= Seq(
      dependencies.generators
    )
  )
  .dependsOn(
    api
  )


lazy val impl = project
  .settings(
    name := "data-entry-service-impl",
    settings,
    libraryDependencies ++= Seq(
      dependencies.hgnc_catalog_api,
      dependencies.icd_catalogs_api,
      dependencies.med_catalog_api
    )
  )
  .dependsOn(
    api
  )


lazy val deps = project
  .in(file("dependencies"))
  .settings(
    name := "data-entry-service-dependencies",
    settings,
    libraryDependencies ++= Seq(
      dependencies.repo_utils,
      dependencies.bwhc_query_api
    )
  )
  .dependsOn(impl)



lazy val tests = project
  .settings(
    name := "tests",
    settings,
    libraryDependencies ++= Seq(
      dependencies.scalatest,
      dependencies.logback,
      dependencies.hgnc_catalog_impl % "test",
      dependencies.icd_catalogs_impl % "test",
      dependencies.med_catalog_impl % "test"
    ),
    publish / skip := true
  )
  .dependsOn(
    api,
    generators % "test",
    impl % "test",
    deps % "test",
  )


//-----------------------------------------------------------------------------
// DEPENDENCIES
//-----------------------------------------------------------------------------

lazy val dependencies =
  new {
    val scalatest          = "org.scalatest"     %% "scalatest"               % "3.1.1" % "test"
    val slf4j              = "org.slf4j"         %  "slf4j-api"               % "1.7.26"
    val logback            = "ch.qos.logback"    %  "logback-classic"         % "1.0.13" % "test"
    val cats_core          = "org.typelevel"     %% "cats-core"               % "2.1.1"
    val play_json          = "com.typesafe.play" %% "play-json"               % "2.8.0"
    val bwhc_utils         = "de.bwhc"           %% "utils"                   % "1.0-SNAPSHOT"
    val generators         = "de.ekut.tbi"       %% "generators"              % "0.1-SNAPSHOT"
    val hgnc_catalog_api   = "de.bwhc"           %% "hgnc-api"                % "1.0-SNAPSHOT"
    val icd_catalogs_api   = "de.bwhc"           %% "icd-catalogs-api"        % "1.0-SNAPSHOT"
    val med_catalog_api    = "de.bwhc"           %% "medication-catalog-api"  % "1.0-SNAPSHOT"
    val hgnc_catalog_impl  = "de.bwhc"           %% "hgnc-impl"               % "1.0-SNAPSHOT"
    val icd_catalogs_impl  = "de.bwhc"           %% "icd-catalogs-impl"       % "1.0-SNAPSHOT"
    val med_catalog_impl   = "de.bwhc"           %% "medication-catalog-impl" % "1.0-SNAPSHOT"
    val repo_utils         = "de.ekut.tbi"       %% "repository-utils"        % "1.0-SNAPSHOT"
    val bwhc_query_api     = "de.bwhc"           %% "query-service-api"       % "1.0-SNAPSHOT"
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

