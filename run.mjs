import { test, field } from "./sample/target/scala-2.12/sample-fastopt/main.mjs";

console.log(field);
const o = test(7);
console.log(o);
console.log(field);
