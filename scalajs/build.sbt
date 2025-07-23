import org.scalajs.linker.interface.{ESVersion, ModuleInitializer, ModuleSplitStyle}
import scala.sys.process.Process
import scala.util.Try

val isScalaSteward = sys.env.get("SCALA_STEWARD").exists { s =>
  val t = s.trim
  t.nonEmpty && Try(t.toInt).getOrElse(1) != 0
}

val stewardScalablyTyped =
  if(isScalaSteward) Seq.empty
  else Seq(ScalablyTypedConverterExternalNpmPlugin)

lazy val squirrelexplorer = project.in(file("."))
  .enablePlugins(ScalaJSPlugin)
  .enablePlugins(stewardScalablyTyped*)
  .settings(
    scalaVersion := "3.7.1",

    // We have two main methods, so we explicitly emit two modules:
    scalaJSUseMainModuleInitializer := false,
    Compile / scalaJSModuleInitializers ++= Seq(
      ModuleInitializer.mainMethodWithArgs("tf.bug.SquirrelExplorer", "main").withModuleID("main"),
      ModuleInitializer.mainMethodWithArgs("tf.bug.worker.SquirrelCompilerWorker", "main").withModuleID("compilerworker"),
      ModuleInitializer.mainMethodWithArgs("tf.bug.worker.SquirrelRendererWorker", "main").withModuleID("rendererworker")
    ),

      /* Configure Scala.js to emit modules in the optimal way to
       * connect to Vite's incremental reload.
       * - emit ECMAScript modules
       * - emit as many small modules as possible for classes in the "tf.bug" package
       * - emit as few (large) modules as possible for all other classes
       *   (in particular, for the standard library)
       */
    scalaJSLinkerConfig ~= {
      _.withESFeatures(_.withESVersion(ESVersion.ES2020))
        .withModuleKind(ModuleKind.ESModule)
        .withModuleSplitStyle(
          ModuleSplitStyle.SmallModulesFor(List("tf.bug")))
    },

    scalacOptions ++= Seq(
      "-no-indent",
      "-old-syntax"
    ),

    libraryDependencies ++= Seq(
      "org.scala-js" %%% "scalajs-dom" % "2.8.0",
      "org.typelevel" %%% "cats-core" % "2.13.0",
      "org.typelevel" %%% "cats-effect" % "3.6.3",
      "co.fs2" %%% "fs2-core" % "3.12.0",
      "com.armanbilge" %%% "calico" % "0.2.3",
      "org.scodec" %%% "scodec-core" % "2.3.2",
      "com.github.plokhotnyuk.jsoniter-scala" %%% "jsoniter-scala-core" % "2.36.7",
      "com.github.plokhotnyuk.jsoniter-scala" %%% "jsoniter-scala-macros" % "2.36.7" % "compile-internal",
    ),

    externalNpm := {
      val pnpm =
        if(System.getProperty("os.name").contains("Windows")) Seq("pwsh", "-c", "pnpm", "install", "--prod=false")
        else Seq("pnpm", "install", "--prod=false")
      Process(pnpm, baseDirectory.value.getParentFile).!
      baseDirectory.value.getParentFile
    },
  )
