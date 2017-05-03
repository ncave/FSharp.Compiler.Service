var path = require("path");
var webpack = require("webpack");

function resolve(filePath) {
  return path.join(__dirname, filePath)
}

var babelOptions = {
  presets: [["es2015", {"modules": false}]]
};

var fableOptions = {
  babel: babelOptions,
  plugins: [],
  define: [
    "COMPILER_SERVICE",
    "FX_NO_CORHOST_SIGNER",
    "FX_NO_LINKEDRESOURCES",
    "FX_NO_PDB_READER",
    "FX_NO_PDB_WRITER",
    "FX_NO_WEAKTABLE",
    "NO_COMPILER_BACKEND",
    "NO_INLINE_IL_PARSER",
    "TRACE"
  ]
};

module.exports = {
  target: 'node',
  //devtool: "source-map",
  entry: resolve('fcs-fable-test.fsproj'),
  output: {
    filename: 'bundle.js',
    path: resolve('./out'),
  },
  module: {
    rules: [
      {
        test: /\.fs(x|proj)?$/,
        use: {
          loader: "fable-loader",
          options: fableOptions
        }
      },
      {
        test: /\.js$/,
        exclude: /node_modules[\\\/](?!fable-)/,
        use: {
          loader: 'babel-loader',
          options: babelOptions
        },
      }
    ]
  }
};
