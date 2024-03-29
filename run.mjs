import { load } from "./loader.mjs";

const moduleExports = await load("./target/sample/output.wasm");
console.log(moduleExports.field);
const o = moduleExports.test(7);
console.log(o);
console.log(moduleExports.field);
