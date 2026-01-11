import { defineConfig } from 'vite'

export default defineConfig({
  optimizeDeps: {
    // Exclude the package that does `new URL('./...core.wasm', import.meta.url)`
    // Use the actual npm package name here:
    exclude: ['@bytecodealliance/jco'],
  },

  // Optional, but can make WASM handling clearer:
  assetsInclude: ['**/*.wasm'],
})