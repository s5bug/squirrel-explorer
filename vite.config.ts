import { defineConfig } from 'vite'
import { resolve } from 'path'
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
export default defineConfig({
  plugins: [
    scalaJS({
      cwd: './scalajs'
    }),
    monacoEditorPlugin({
      languageWorkers: ['editorWorkerService']
    })
  ]
})
