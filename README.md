## Experimental Scala.js linker backend for WasmGC

### Usage

- `sample/run` to compile `sample/src/main/scala/Sample.scala` to WebAssembly Text Format (WAT) (Stack IR form)
- Copy and paste the output WAT to a file, and transform it to binary using WasmGC reference interpreter.
  - https://github.com/WebAssembly/gc/tree/main/interpreter
  - Use docker image for it https://github.com/tanishiking/wasmgc-docker


Run the binary using Deno or something (`run.mjs`).

```js
import { readFileSync } from "node:fs";
const wasmBuffer = readFileSync("test.wasm");
const wasmModule = await WebAssembly.instantiate(wasmBuffer);
const { bar } = wasmModule.instance.exports;
const o = bar(100);
console.log(o);
```

```sh
$ deno run --allow-read run.mjs
```


