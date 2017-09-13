import fable from 'rollup-plugin-fable';
const fableUtils = require("fable-utils");

// const babelOptions = fableUtils.resolveBabelOptions({
//   presets: [["es2015", {"modules": false}]]
// });

const fableOptions = {
  //babel: babelOptions,
  //plugins: [],
  define: [
    "COMPILER_PUBLIC_API",
    "FX_NO_CORHOST_SIGNER",
    "FX_NO_LINKEDRESOURCES",
    "FX_NO_PDB_READER",
    "FX_NO_PDB_WRITER",
    "FX_NO_WEAKTABLE",
    "FX_REDUCED_EXCEPTIONS",
    "NO_COMPILER_BACKEND",
    "NO_INLINE_IL_PARSER"
  ]
};

export default {
  input: './fcs-fable-test.fsproj',
  output: {
    file: './out/bundle.js',
    format: 'cjs', // 'amd', 'cjs', 'es', 'iife', 'umd'
  },
  plugins: [
    fable(fableOptions),
  ],
};
