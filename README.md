## Experimental Scala.js linker backend for WasmGC

[![CI](https://github.com/tanishiking/scala-wasm/actions/workflows/ci.yml/badge.svg)](https://github.com/tanishiking/scala-wasm/actions/workflows/ci.yml)

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

### Testing

Requires NodeJS >= 22 (for enough support of WasmGC).

```sh
$ NVM_NODEJS_ORG_MIRROR=https://nodejs.org/download/nightly nvm install v22
```

- `tests/test` will
  - Run `testSuite/run` to compile the Scala code under `test-suite` to WebAssembly
  - Run the WebAssembly binary using NodeJS
- Each Scala program in `test-suite` should have a function that has no arguments and return a Boolean value. The test passes if the function returns `true`.
- When you add a test, 
  - Add a file under `test-suite`
  - Add a test case to `cli/src/main/scala/TestSuites.scala` (`methodName` should be a exported function name).
