import { readFileSync } from "node:fs";
const wasmBuffer = readFileSync("./target/output.wasm");
const wasmModule = await WebAssembly.instantiate(wasmBuffer);
const { test } = wasmModule.instance.exports;
const o = test();
console.log(o);
