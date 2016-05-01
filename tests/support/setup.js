var glob = require("glob");
var fs = require("fs");
var rimraf = require("rimraf");

rimraf.sync("flattened");
rimraf.sync("prelude");

var mkdirTry = function (dir) {
  try {
    fs.mkdirSync(dir);
  } catch(e) {
    // ignore the error if it already exists
    if (e.code !== "EEXIST") {
      throw(e);
    }
  }
};

mkdirTry("flattened");

glob("bower_components/*/src/**/*.{js,purs}", function(err, files) {
  if (err) throw err;
  files.forEach(function(file) {
    // We join with "-" because Cabal is weird about file extensions.
    var dest = "flattened/" + file.split("/").slice(3).join("-");
    console.log("Copying " + file + " to " + dest);
    var content = fs.readFileSync(file, "utf-8");
    fs.writeFileSync(dest, content, "utf-8");
  });
});

glob("bower_components/purescript-prelude/**/*", function(err, paths) {
  if (err) throw err;
  paths
    .filter(function (path) {
      return fs.statSync(path).isFile();
    }).forEach(function(file) {
      var dest = "prelude/" + file.split("/").slice(2).join("/");
      dest.split("/").reduce(function (path, part) {
        mkdirTry(path);
        return path + "/" + part;
      });
      console.log("Copying " + file + " to " + dest);
      var content = fs.readFileSync(file, "utf-8");
      fs.writeFileSync(dest, content, "utf-8");
    });
});
