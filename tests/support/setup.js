var glob = require("glob");
var fs = require("fs");

try {
  fs.mkdirSync("./flattened");
} catch(e) {
  // ignore the error if it already exists
  if (e.code !== "EEXIST") {
    throw(e);
  }
}

glob("bower_components/*/src/**/*.{js,purs}", function(err, files) {
  if (err) throw err;
  files.forEach(function(file) {
    // We join with "-" because Cabal is weird about file extensions.
    var dest = "./flattened/" + file.split("/").slice(3).join("-");
    console.log("Copying " + file + " to " + dest);
    var content = fs.readFileSync(file, "utf-8");
    fs.writeFileSync(dest, content, "utf-8");
  });
})
