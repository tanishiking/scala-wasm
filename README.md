## Experimental Scala.js linker backend for WasmGC

[![CI](https://github.com/tanishiking/scala-wasm/actions/workflows/ci.yml/badge.svg)](https://github.com/tanishiking/scala-wasm/actions/workflows/ci.yml)

### Prerequisites

This project requires Node.js >= 22, which is available as nightly builds as of this writing.
This is necessary to get enough support of WasmGC.

If you are using NVM, you can instal Node.js 22 as follows:

```sh
# Enable resolution of nightly builds
$ NVM_NODEJS_ORG_MIRROR=https://nodejs.org/download/nightly nvm install v22
# Switch to Node.js 22
$ nvm use 22
```

Otherwise, you can [manually download nightly builds of Node.js](https://nodejs.org/download/nightly/).
You will need a nightly of v22 from 2024/04/01 or later.

### Setup

Before doing anything else, run `npm install`.

### Run the sample

In `sample/src/main/scala/Sample.scala` you can find a sandbox to play around.

You can build and run it like any other Scala.js project from sbt:

- `sample/fastLinkJS` compiles and links the project with the WebAssembly backend.
- `sample/run` runs the sample linked to WebAssembly with `node`.

You may want to look at the output in `sample/target/scala-2.12/sample-fastopt/` to convince yourself that it was compiled to WebAssembly.

In that directory, you will also find a `main.wat` file, which is not used for execution.
It contains the WebAsembly Text Format representation of `main.wasm`, for exploratory and debugging purposes.
This is only true by default for the `sample`.

:warning: If you modify the linker code, you need to `reload` and `sample/clean` for your changes to take effect on the sample.

You can also use the `run.mjs` script to play with `@JSExportTopLevel` exports.

- Run from the command line with `node --experimental-wasm-exnref run.mjs`.
- Run from the command line with `DENO_V8_FLAGS=--experimental-wasm-exnref deno run --allow-read run.mjs`.
- Run from the browser by starting an HTTP server (e.g., `python -m http.server`) and navigate to `testrun.html`.

If you encounter the `Invalid opcode 0x1f` error with Node.js, you need to use a more recent nightly build (minimum required: v22 nightly from 2024/04/01).

### Unit test suite

Run the unit test suite with `tests/test`.

- `tests/test` will
  - Link every test suite from `testSuite` with the WebAssembly backend
  - Run the produced WebAssembly module and check for any uncaught error
- Each Scala program in `test-suite` should have a `def main(): Unit` function. The test passes if the function successfully executes without throwing.
- When you add a test,
  - Add a file under `test-suite`
  - Add a test case to `tests/src/test/scala/tests/TestSuites.scala`

By default, `.wat` files are not generated, as they are quite big (several hundreds of KB for most of the tests).
You can enable them by adding `withPrettyPrint(true)` to the linker configuration in `tests/src/test/scala/tests/CoreTests.scala`.

### Scala.js integration test suite

Run the entire Scala.js test suite, linked with the WebAssembly backend, with:

```
> scalajs-test-suite/test
```

When you modify the linker, you need to `reload` and `scalajs-test-suite/clean` for your changes to take effect.
Since recompiling the test suite from scratch every time is slow, you can replace the `clean` by manually removing only the linker output with:

```
$ rm -r scalajs-test-suite/target/scala-2.12/scalajs-test-suite-test-fastopt/
```

By default, `.wat` files are not generated for the Scala.js test suite, as they are very big (they exceed 100 MB).
It is usually easier to minimize an issue in `sample/test`, but if you really want the big `.wat` file for the Scala.js test suite, you can enable it with

```scala
> set `scalajs-test-suite`/scalaJSLinkerConfig ~= { _.withPrettyPrint(true) }
```

### Debugging tools

- The WasmGC reference interpreter can be used to validate and convert between the binary and text form:
  - https://github.com/WebAssembly/gc/tree/main/interpreter
  - Use docker image for it https://github.com/tanishiking/wasmgc-docker
