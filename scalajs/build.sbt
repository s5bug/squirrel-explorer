import org.scalajs.linker.interface.{ModuleInitializer, ModuleSplitStyle}
import scala.sys.process.Process

lazy val squirrelexplorer = project.in(file("."))
  .enablePlugins(ScalaJSPlugin, ScalablyTypedConverterExternalNpmPlugin)
  .settings(
    scalaVersion := "3.5.0",

    // We have two main methods, so we explicitly emit two modules:
    scalaJSUseMainModuleInitializer := false,
    Compile / scalaJSModuleInitializers ++= Seq(
      ModuleInitializer.mainMethodWithArgs("tf.bug.fe.SquirrelExplorer", "main").withModuleID("main"),
      ModuleInitializer.mainMethodWithArgs("tf.bug.worker.SquirrelWorker", "main").withModuleID("worker")
    ),

      /* Configure Scala.js to emit modules in the optimal way to
       * connect to Vite's incremental reload.
       * - emit ECMAScript modules
       * - emit as many small modules as possible for classes in the "tf.bug" package
       * - emit as few (large) modules as possible for all other classes
       *   (in particular, for the standard library)
       */
    scalaJSLinkerConfig ~= {
      _.withModuleKind(ModuleKind.ESModule)
        .withModuleSplitStyle(
          ModuleSplitStyle.SmallModulesFor(List("tf.bug")))
    },

    scalacOptions ++= Seq(
      "-no-indent",
      "-old-syntax"
    ),

    libraryDependencies ++= Seq(
      "org.scala-js" %%% "scalajs-dom" % "2.8.0",
      "org.typelevel" %%% "cats-core" % "2.12.0",
      "org.typelevel" %%% "cats-effect" % "3.5.4",
      "co.fs2" %%% "fs2-core" % "3.11.0",
      "com.armanbilge" %%% "calico" % "0.2.2",
      "org.scodec" %%% "scodec-core" % "2.3.1",
      "org.typelevel" %%% "paiges-core" % "0.4.4",
    ),

    externalNpm := {
      val pnpm =
        if(System.getProperty("os.name").contains("Windows")) Seq("pwsh", "-c", "pnpm", "install", "--prod=false")
        else Seq("pnpm", "install", "--prod=false")
      Process(pnpm, baseDirectory.value.getParentFile).!
      baseDirectory.value.getParentFile
    },
  )
