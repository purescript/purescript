"use strict";

var foo = 1;

exports.bar = { foo };

var baz = 2;

exports.quux = function(baz) {
  return { baz };
};

var fs = require('fs');
var source = fs.readFileSync(__filename, 'utf-8');
exports.bazIsEliminated = !/^ *var baz =/m.test(source);
