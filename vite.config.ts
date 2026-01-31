import {defineConfig, loadEnv, PluginOption} from 'vite'
import path from 'path'
import scalaJS from '@scala-js/vite-plugin-scalajs'
import monacoEditorPluginModule from 'vite-plugin-monaco-editor'
import {execSync} from "node:child_process";

const isObjectWithDefaultFunction = (module: unknown): module is { default: typeof monacoEditorPluginModule } => (
  module != null &&
  typeof module === 'object' &&
  'default' in module &&
  typeof module.default === 'function'
)

const monacoEditorPlugin = isObjectWithDefaultFunction(monacoEditorPluginModule)
  ? monacoEditorPluginModule.default
  : monacoEditorPluginModule

const mesonBuildWasmPlugin: () => PluginOption = () => {
  return {
    name: 'meson-build-wasm',
    buildStart() {
      const buildDir = "build"

      let crossfile = "emscripten.cross";
      if(process.platform == "win32") crossfile = "emscripten-windows.cross";

      let buildtype = "-Dbuildtype=debug";
      if(process.env.NODE_ENV == "production") buildtype = "-Dbuildtype=release";

      execSync(
        `emmake meson setup ${buildDir} --cross-file ./${crossfile} --reconfigure ${buildtype}`,
        {cwd: "wasm"}
      )
      execSync(
        `emmake meson compile -vC ${buildDir}`, {cwd: "wasm"}
      )
    }
  }
}

// https://vitejs.dev/config/
export default defineConfig(({ command, mode }) => {
  const env = loadEnv(mode, process.cwd(), '')
  let basePath;
  if(env["GITHUB_ACTIONS"] === "true") {
    basePath = "/" + env["GITHUB_REPOSITORY"].split("/")[1]
  } else {
    basePath = "/"
  }
  const monacoWorkerPath = "monacoeditorwork"
  return {
    base: basePath,
    resolve: {
      alias: {
        "@": path.resolve(__dirname, "./src/"),
      },
    },
    plugins: [
      mesonBuildWasmPlugin(),
      scalaJS({
        cwd: './scalajs'
      }),
      monacoEditorPlugin({
        languageWorkers: ['editorWorkerService'],
        publicPath: monacoWorkerPath,
        customDistPath: (root: string, buildOutDir: string, base: string) => {
          return path.join(root, buildOutDir, /* don't join base, */ monacoWorkerPath)
        }
      })
    ],
    worker: {
      format: 'iife'
    }
  }
})
