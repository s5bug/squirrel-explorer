import { defineConfig, loadEnv } from 'vite'
import { resolve, join } from 'path'
import scalaJS from '@scala-js/vite-plugin-scalajs'
import monacoEditorPluginModule from 'vite-plugin-monaco-editor'

const isObjectWithDefaultFunction = (module: unknown): module is { default: typeof monacoEditorPluginModule } => (
  module != null &&
  typeof module === 'object' &&
  'default' in module &&
  typeof module.default === 'function'
)

const monacoEditorPlugin = isObjectWithDefaultFunction(monacoEditorPluginModule)
  ? monacoEditorPluginModule.default
  : monacoEditorPluginModule

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
    plugins: [
      scalaJS({
        cwd: './scalajs'
      }),
      monacoEditorPlugin({
        languageWorkers: ['editorWorkerService'],
        publicPath: monacoWorkerPath,
        customDistPath: (root: string, buildOutDir: string, base: string) => {
          return join(root, buildOutDir, /* don't join base, */ monacoWorkerPath)
        }
      })
    ]
  }
})
