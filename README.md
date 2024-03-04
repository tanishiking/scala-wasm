## Experimental Scala.js linker backend for WasmGC

### Usage

- `sample/run` to compile `sample/src/main/scala/Sample.scala`.
  - prints the WebAssembly Text Format (WAT) (Stack IR form) to the console, for debugging
  - writes the binary format (WASM) to `target/output.wasm`

Run the binary using through `run.js` using a JavaScript engine that supports WasmGC, such as Deno

```sh
$ deno run --allow-read run.mjs
```

### Debugging tools

- The WasmGC reference interpreter can be used to validate and convert between the binary and text form:
  - https://github.com/WebAssembly/gc/tree/main/interpreter
  - Use docker image for it https://github.com/tanishiking/wasmgc-docker
