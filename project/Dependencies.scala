import org.portablescala.sbtplatformdeps.PlatformDepsPlugin.autoImport._
import org.scalajs.jsdependencies.sbtplugin.JSDependenciesPlugin.autoImport._
import sbt._

object Dependencies {
  val versionOfScala = "2.13.10"

  // Udash
  val udashVersion = "0.9.0-M24"
  val udashJQueryVersion = "3.0.4"

  // Backend
  val jettyVersion = "9.4.40.v20210413"
  val logbackVersion = "1.2.8"
  val typesafeConfigVersion = "1.4.1"
  val quicklensVersion = "1.7.4"
  val betterFilesVersion = "3.9.1"
  val zioJsonVersion = "0.1.5"
  val macwireVersion = "2.5.7"

  // JS dependencies
  val bootstrapVersion = "5.2.3"

  // Testing
  val scalatestVersion = "3.2.3"
  val scalamockVersion = "5.1.0"

  // Dependencies for both frontend and backend
  // Those have to be cross-compilable
  val crossDeps = Def.setting(
    Seq(
      "io.udash" %%% "udash-core" % udashVersion,
      "io.udash" %%% "udash-rpc" % udashVersion,
      "io.udash" %%% "udash-rest" % udashVersion,
      "io.udash" %%% "udash-i18n" % udashVersion,
      "io.udash" %%% "udash-css" % udashVersion,
      "io.udash" %%% "udash-auth" % udashVersion,
      "com.softwaremill.quicklens" %%% "quicklens" % quicklensVersion,
      "dev.zio" %%% "zio-json" % zioJsonVersion
    )
  )

  // Dependencies compiled to JavaScript code
  val frontendDeps = Def.setting(
    Seq(
      "io.udash" %%% "udash-core" % udashVersion,
      "io.udash" %%% "udash-rpc" % udashVersion,
      "io.udash" %%% "udash-i18n" % udashVersion,
      "io.udash" %%% "udash-css" % udashVersion,
      "io.udash" %%% "udash-auth" % udashVersion,

      // type-safe wrapper for Twitter Bootstrap
      "io.udash" %%% "udash-bootstrap4" % udashVersion,
      // type-safe wrapper for jQuery
      "io.udash" %%% "udash-jquery" % udashJQueryVersion,

      // other dependencies
      "com.softwaremill.macwire" %%% "macros" % macwireVersion % "provided"
    )
  )

  // JavaScript libraries dependencies
  // Those will be added into frontend-deps.js
  val frontendJSDeps = Def.setting(
    Seq(
      // "jquery.js" is provided by "udash-jquery" dependency
      "org.webjars" % "bootstrap" % bootstrapVersion / "js/bootstrap.bundle.js"
        minified "js/bootstrap.bundle.min.js" dependsOn "jquery.js"
    )
  )

  // Dependencies for JVM part of code
  val backendDeps = Def.setting(
    Seq(
      "io.udash" %% "udash-rpc" % udashVersion,
      "io.udash" %% "udash-rest" % udashVersion,
      "io.udash" %% "udash-i18n" % udashVersion,
      "io.udash" %% "udash-css" % udashVersion,
      "org.eclipse.jetty" % "jetty-server" % jettyVersion,
      "org.eclipse.jetty" % "jetty-rewrite" % jettyVersion,
      "org.eclipse.jetty.websocket" % "websocket-server" % jettyVersion,
      "com.typesafe" % "config" % typesafeConfigVersion,
      "com.github.pathikrit" %% "better-files" % betterFilesVersion,
      // server logging backend
      "ch.qos.logback" % "logback-classic" % logbackVersion
    )
  )

  // Test dependencies
  val crossTestDeps = Def.setting(
    Seq(
      "org.scalatest" %%% "scalatest" % scalatestVersion,
      "org.scalamock" %%% "scalamock" % scalamockVersion
    ).map(_ % Test)
  )

}
