import { load } from "./loader.mjs";

const { test } = await load("./target/output.wasm");
const o = test();
console.log(o);
