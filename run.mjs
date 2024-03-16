import { load } from "./loader.mjs";

const { test } = await load("./target/output.wasm");
const o = test(7);
console.log(o);
