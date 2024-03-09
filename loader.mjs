import { readFileSync } from "node:fs";

// Specified by java.lang.String.hashCode()
function stringHashCode(s) {
  var res = 0;
  var mul = 1;
  var i = (s.length - 1) | 0;
  while ((i >= 0)) {
    res = ((res + Math.imul(s.charCodeAt(i), mul)) | 0);
    mul = Math.imul(31, mul);
    i = (i - 1) | 0;
  }
  return res;
}

const scalaJSHelpers = {
  // BinaryOp.===
  is: Object.is,

  // undefined
  undef: () => void 0,
  isUndef: (x) => x === (void 0),

  // Boxes (upcast) -- most are identity at the JS level but with different types in Wasm
  bZ: (x) => x !== 0,
  bB: (x) => x,
  bS: (x) => x,
  bI: (x) => x,
  bF: (x) => x,
  bD: (x) => x,

  // Unboxes (downcast, null is converted to the zero of the type)
  uZ: (x) => x | 0,
  uB: (x) => (x << 24) >> 24,
  uS: (x) => (x << 16) >> 16,
  uI: (x) => x | 0,
  uF: (x) => Math.fround(x),
  uD: (x) => +x,

  // Unboxes to primitive or null (downcast to the boxed classes)
  uNZ: (x) => (x !== null) ? (x | 0) : null,
  uNB: (x) => (x !== null) ? ((x << 24) >> 24) : null,
  uNS: (x) => (x !== null) ? ((x << 16) >> 16) : null,
  uNI: (x) => (x !== null) ? (x | 0) : null,
  uNF: (x) => (x !== null) ? Math.fround(x) : null,
  uND: (x) => (x !== null) ? +x : null,

  // Type tests
  tZ: (x) => typeof x === 'boolean',
  tB: (x) => typeof x === 'number' && Object.is((x << 24) >> 24, x),
  tS: (x) => typeof x === 'number' && Object.is((x << 16) >> 16, x),
  tI: (x) => typeof x === 'number' && Object.is(x | 0, x),
  tF: (x) => typeof x === 'number' && (Math.fround(x) === x || x !== x),
  tD: (x) => typeof x === 'number',

  // Strings
  emptyString: () => "",
  stringLength: (s) => s.length,
  stringCharAt: (s, i) => s.charCodeAt(i),
  jsValueToString: (x) => "" + x,
  booleanToString: (b) => b ? "true" : "false",
  charToString: (c) => String.fromCharCode(c),
  intToString: (i) => "" + i,
  doubleToString: (d) => "" + d,
  stringConcat: (x, y) => ("" + x) + y, // the added "" is for the case where x === y === null
  isString: (x) => typeof x === 'string',

  // Hash code, because it is overridden in all hijacked classes
  // Specified by the hashCode() method of the corresponding hijacked classes
  jsValueHashCode: (x) => {
    if (typeof x === 'number')
      return x | 0; // TODO make this compliant for floats
    if (typeof x === 'string')
      return stringHashCode(x);
    if (typeof x === 'boolean')
      return x ? 1231 : 1237;
    if (typeof x === 'undefined')
      return 0;
    return 42; // for any JS object
  },
}

export async function load(wasmFileName) {
  const wasmBuffer = readFileSync(wasmFileName);
  const wasmModule = await WebAssembly.instantiate(wasmBuffer, {
    "__scalaJSHelpers": scalaJSHelpers,
  });
  return wasmModule.instance.exports;
}
