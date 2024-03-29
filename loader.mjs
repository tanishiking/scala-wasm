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

const linkingInfo = Object.freeze({
  "esVersion": 6,
  "assumingES6": true,
  "productionMode": false,
  "linkerVersion": "1.15.0",
  "fileLevelThis": this
});

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

  // Closure
  closure: (f, data) => f.bind(void 0, data),
  closureThis: (f, data) => function(...args) { return f(this, data, ...args); },
  closureRest: (f, data, n) => ((...args) => f(data, ...args.slice(0, n), args.slice(n))),
  closureThisRest: (f, data, n) => function(...args) { return f(this, data, ...args.slice(0, n), args.slice(n)); },

  // Strings
  emptyString: () => "",
  stringLength: (s) => s.length,
  stringCharAt: (s, i) => s.charCodeAt(i),
  jsValueToString: (x) => "" + x,
  booleanToString: (b) => b ? "true" : "false",
  charToString: (c) => String.fromCharCode(c),
  intToString: (i) => "" + i,
  longToString: (l) => "" + l, // l must be a bigint here
  doubleToString: (d) => "" + d,
  stringConcat: (x, y) => ("" + x) + y, // the added "" is for the case where x === y === null
  isString: (x) => typeof x === 'string',

  /* Get the type of JS value of `x` in a single JS helper call, for the purpose of dispatch.
   *
   * 0: false
   * 1: true
   * 2: string
   * 3: number
   * 4: undefined
   * 5: everything else
   *
   * This encoding has the following properties:
   *
   * - false and true also return their value as the appropriate i32.
   * - the types implementing `Comparable` are consecutive from 0 to 3.
   */
  jsValueType: (x) => {
    if (typeof x === 'number')
      return 3;
    if (typeof x === 'string')
      return 2;
    if (typeof x === 'boolean')
      return x | 0;
    if (typeof x === 'undefined')
      return 4;
    return 5;
  },

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

  // JS interop
  jsGlobalRefGet: (globalRefName) => (new Function("return " + globalRefName))(),
  jsGlobalRefSet: (globalRefName, v) => {
    var argName = globalRefName === 'v' ? 'w' : 'v';
    (new Function(argName, globalRefName + " = " + argName))(v);
  },
  jsGlobalRefTypeof: (globalRefName) => (new Function("return typeof " + globalRefName))(),
  jsNewArray: () => [],
  jsArrayPush: (a, v) => (a.push(v), a),
  jsArraySpreadPush: (a, vs) => (a.push(...vs), a),
  jsNewObject: () => ({}),
  jsObjectPush: (o, p, v) => (o[p] = v, o),
  jsSelect: (o, p) => o[p],
  jsSelectSet: (o, p, v) => o[p] = v,
  jsNew: (constr, args) => new constr(...args),
  jsFunctionApply: (f, args) => f(...args),
  jsMethodApply: (o, m, args) => o[m](...args),
  jsDelete: (o, p) => { delete o[p]; },
  jsIsTruthy: (x) => !!x,
  jsLinkingInfo: () => linkingInfo,

  // Excruciating list of all the JS operators
  jsUnaryPlus: (a) => +a,
  jsUnaryMinus: (a) => -a,
  jsUnaryTilde: (a) => ~a,
  jsUnaryBang: (a) => !a,
  jsUnaryTypeof: (a) => typeof a,
  jsStrictEquals: (a, b) => a === b,
  jsNotStrictEquals: (a, b) => a !== b,
  jsPlus: (a, b) => a + b,
  jsMinus: (a, b) => a - b,
  jsTimes: (a, b) => a * b,
  jsDivide: (a, b) => a / b,
  jsModulus: (a, b) => a % b,
  jsBinaryOr: (a, b) => a | b,
  jsBinaryAnd: (a, b) => a & b,
  jsBinaryXor: (a, b) => a ^ b,
  jsShiftLeft: (a, b) => a << b,
  jsArithmeticShiftRight: (a, b) => a >> b,
  jsLogicalShiftRight: (a, b) => a >>> b,
  jsLessThan: (a, b) => a < b,
  jsLessEqual: (a, b) => a <= b,
  jsGreaterThan: (a, b) => a > b,
  jsGreaterEqual: (a, b) => a >= b,
  jsIn: (a, b) => a in b,
  jsInstanceof: (a, b) => a instanceof b,
  jsExponent: (a, b) => a ** b,
}

export async function load(wasmFileName) {
  const importsObj = {
    "__scalaJSHelpers": scalaJSHelpers,
  };
  var wasmModulePromise;
  if (typeof process !== "undefined") {
    wasmModulePromise = import("node:fs").then((fs) => {
      return WebAssembly.instantiate(fs.readFileSync(wasmFileName), importsObj);
    });
  } else {
    wasmModulePromise = WebAssembly.instantiateStreaming(fetch(wasmFileName), importsObj);
  }
  const wasmModule = await wasmModulePromise;
  const exports = wasmModule.instance.exports;

  const userExports = Object.create(null);
  for (const exportName of Object.getOwnPropertyNames(exports)) {
    const exportValue = exports[exportName];
    if (exportValue instanceof WebAssembly.Global) {
      Object.defineProperty(userExports, exportName, {
        configurable: true,
        enumerable: true,
        get: () => exportValue.value,
      });
    } else {
      userExports[exportName] = exportValue;
    }
  }
  Object.freeze(userExports);
  return userExports;
}
