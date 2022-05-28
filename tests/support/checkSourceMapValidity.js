// Run as `node checkSourceMapValidity.js path/to/index.js.map`

const s = require("source-map");
const fs = require("fs");
const process = require("process");

if (process.argv.length < 3) {
  const errMsg = [
    "Script did not receive the source map file path as its only argument",
    "Rerun using `node checkSourceMapValidity.js path/to/index.js.map`"
  ].join("\n");
  throw new Error(errMsg);
}

const sourceMapFilePath = process.argv[2];
console.log(`Checking validity of source map for ${sourceMapFilePath}`);
const content = fs.readFileSync(sourceMapFilePath, {encoding: "utf-8"});
s.SourceMapConsumer.with(
    JSON.parse(content),
    null,
    (consumer) => {
      // We only use the `eachMapping` function to trigger an error
      // if a mapping is invalid.
      consumer.eachMapping(function () {});
    }
  )
  .then(() => console.log(`${sourceMapFilePath} sourcemap is valid`))
  .catch((e) => {
    console.log(`  ${e.message}`);
    // See https://nodejs.org/dist/latest-v16.x/docs/api/process.html#processexitcode
    // for why we don't call `process.exit(1)`
    process.exitCode = 1;
  });