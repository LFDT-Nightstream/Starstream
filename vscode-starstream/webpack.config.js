//@ts-check
// https://code.visualstudio.com/api/working-with-extensions/bundling-extension#configure-webpack
"use strict";

import { resolve } from "path";
import webpack from "webpack";

const typescriptRule = {
  test: /\.ts$/,
  exclude: /node_modules/,
  use: [
    {
      loader: "ts-loader",
    },
  ],
};

// We're using file-loader because we have to handle filenames ourselves
// in the VSC-web context, so turn off the default Asset Modules to avoid
// duplicate copies:
// https://webpack.js.org/guides/asset-modules/#disable-emitting-assets
const noDefaultAssetModules = {
  test: /\.wasm$/,
  generator: {
    emit: false,
  }
};

/**@type {import('webpack').Configuration[]}*/
export default [
  // Language server worker export.
  {
    target: "webworker",
    entry: "./src/language-server.worker.ts",
    output: {
      path: resolve(import.meta.dirname, "dist"),
      filename: "language-server.worker.js",
      devtoolModuleFilenameTemplate: "../[resource-path]",
    },
    devtool: "nosources-source-map",
    resolve: {
      mainFields: ["browser", "module", "main"],
      extensions: [".ts", ".js"],
    },
    experiments: {
      asyncWebAssembly: true,
    },
    module: {
      rules: [typescriptRule, noDefaultAssetModules],
    },
  },
  // Main extension export.
  {
    target: "webworker",
    // vscode extensions run in webworker context for VS Code web 📖 -> https://webpack.js.org/configuration/target/#target
    // but the webworker version of vscode-languageclient of course doesn't let you spawn processes, so it's more work if we want that to work

    entry: "./src/extension.ts", // the entry point of this extension, 📖 -> https://webpack.js.org/configuration/entry-context/
    output: {
      // the bundle is stored in the 'dist' folder (check package.json), 📖 -> https://webpack.js.org/configuration/output/
      path: resolve(import.meta.dirname, "dist"),
      filename: "extension.js",
      publicPath: "",
      libraryTarget: "commonjs2",
      devtoolModuleFilenameTemplate: "../[resource-path]",
    },
    devtool: "nosources-source-map",
    externals: {
      vscode: "commonjs vscode", // the vscode-module is created on-the-fly and must be excluded. Add other modules that cannot be webpack'ed, 📖 -> https://webpack.js.org/configuration/externals/
    },
    resolve: {
      // support reading TypeScript and JavaScript files, 📖 -> https://github.com/TypeStrong/ts-loader
      mainFields: ["browser", "module", "main"], // look for `browser` entry point in imported node modules
      extensions: [".ts", ".js"],
      alias: {
        // provides alternate implementation for node module and source files
      },
      fallback: {
        // Webpack 5 no longer polyfills Node.js core modules automatically.
        // see https://webpack.js.org/configuration/resolve/#resolvefallback
        // for the list of Node.js core module polyfills.
      },
    },
    plugins: [
      // Ignore stuff that `web-tree-sitter` tries to conditionally import on Node in a way that webpack doesn't get.
      new webpack.IgnorePlugin({
        resourceRegExp: /^(path|fs|fs\/promises|module)$/,
      }),
    ],
    // experiments: {
    //   asyncWebAssembly: true,
    // },
    module: {
      rules: [typescriptRule, noDefaultAssetModules],
    },
  },
];
