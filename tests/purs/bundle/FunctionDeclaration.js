"use strict";

var foo = 0;

function bar(foo) {
  return foo;
}

var baz = "Done";

function qux() {
  return bar(baz);
}

exports.qux = qux;

var fs = require('fs');
var source = fs.readFileSync(__filename, 'utf-8');
exports.fooIsEliminated = !/^ *var foo/m.test(source);

function localFunction() {
  return true;
}

localFunction(); // this should cause localFunction to be bundled