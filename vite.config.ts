import {defineConfig, loadEnv, PluginOption} from 'vite'
import path from 'node:path'
import {lezer} from '@lezer/generator/rollup'
import scalaJS from '@scala-js/vite-plugin-scalajs'
import {execSync} from 'node:child_process'

const mesonBuildWasmPlugin: () => PluginOption = () => {
  return {
    name: 'meson-build-wasm',
    buildStart() {
      const buildDir = "buildDir"

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
export default defineConfig(({ mode }) => {
  const env = loadEnv(mode, process.cwd(), '')
  let basePath;
  if(env["GITHUB_ACTIONS"] === "true") {
    basePath = "/" + env["GITHUB_REPOSITORY"].split("/")[1]
  } else {
    basePath = "/"
  }
  return {
    base: basePath,
    resolve: {
      alias: {
        "@": path.resolve(__dirname, "./src/"),
      },
    },
    plugins: [
      mesonBuildWasmPlugin(),
      lezer(),
      scalaJS({
        cwd: './scalajs'
      }),
    ],
    worker: {
      format: 'iife'
    }
  }
})
