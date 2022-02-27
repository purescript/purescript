export var foo = 1;

export var bar = { foo };

var baz = 2;

export var quux = function(baz) {
  return { baz };
};

import * as fs from 'fs';
var source = fs.readFileSync(__filename, 'utf-8');
export var bazIsEliminated = !/^ *var baz =/m.test(source);
