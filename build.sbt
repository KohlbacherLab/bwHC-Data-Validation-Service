
/*
 build.sbt adapted from https://github.com/pbassiner/sbt-multi-project-example/blob/master/build.sbt
*/


name := "bwhc-data-entry-service"
ThisBuild / organization := "de.bwhc"
ThisBuild / scalaVersion := "2.13.8"
ThisBuild / version      := "1.1-SNAPSHOT"


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
     impl,
     deps,
     tests
  )


lazy val api = project
  .settings(
    name := "data-entry-service-api",
    settings,
    libraryDependencies ++= Seq(
      dependencies.query_api
    )
  )


lazy val impl = project
  .settings(
    name := "data-entry-service-impl",
    settings,
    libraryDependencies ++= Seq(
      dependencies.scalatest,
      dependencies.mtb_dto_gens,
      dependencies.hgnc_catalog_impl,
      dependencies.icd_catalogs_impl,
      dependencies.med_catalog_impl
    )
  )
  .dependsOn(
    api,
  )


lazy val deps = project
  .in(file("dependencies"))
  .settings(
    name := "data-entry-service-dependencies",
    settings,
    libraryDependencies ++= Seq(
      dependencies.repo_utils,
    )
  )
  .dependsOn(impl)



lazy val tests = project
  .settings(
    name := "tests",
    settings,
    libraryDependencies ++= Seq(
      dependencies.scalatest,
      dependencies.slf4j,
      dependencies.mtb_dto_gens,
      dependencies.hgnc_catalog_impl,
      dependencies.icd_catalogs_impl,
      dependencies.med_catalog_impl
    ),
    publish / skip := true
  )
  .dependsOn(
    api,
    impl % Test,
    deps % Test,
  )


//-----------------------------------------------------------------------------
// DEPENDENCIES
//-----------------------------------------------------------------------------

lazy val dependencies =
  new {
    val scalatest          = "org.scalatest"     %% "scalatest"               % "3.1.1" % Test
    val slf4j              = "org.slf4j"         %  "slf4j-api"               % "1.7.32"
    val play_json          = "com.typesafe.play" %% "play-json"               % "2.8.1"
    val repo_utils         = "de.ekut.tbi"       %% "repository-utils"        % "1.0-SNAPSHOT" 
    val mtb_dto_gens       = "de.bwhc"           %% "mtb-dto-generators"      % "1.0-SNAPSHOT" % Test
    val query_api          = "de.bwhc"           %% "query-service-api"       % "1.1-SNAPSHOT"
    val hgnc_catalog_impl  = "de.bwhc"           %% "hgnc-impl"               % "1.0" % Test
    val icd_catalogs_impl  = "de.bwhc"           %% "icd-catalogs-impl"       % "1.1" % Test
    val med_catalog_impl   = "de.bwhc"           %% "medication-catalog-impl" % "1.0" % Test
  }


//-----------------------------------------------------------------------------
// SETTINGS
//-----------------------------------------------------------------------------

lazy val settings = commonSettings


lazy val compilerOptions = Seq(
  "-encoding", "utf8",
  "-unchecked",
  "-feature",
  "-language:postfixOps",
  "-Xfatal-warnings",
  "-deprecation",
)

lazy val commonSettings = Seq(
  scalacOptions ++= compilerOptions,
  resolvers ++= Seq("Local Maven Repository" at "file://" + Path.userHome.absolutePath + "/.m2/repository") ++
    Resolver.sonatypeOssRepos("releases") ++
    Resolver.sonatypeOssRepos("snapshots")
)

