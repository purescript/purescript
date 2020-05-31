"use strict";

var fs = require('fs');

var source = fs.readFileSync(__filename, 'utf-8');

exports.results = {
    fooIsNotEliminated: /^ *var foo =/m.test(source),
    barIsExported: /^ *exports\["bar"\] =/m.test(source),
    barIsNotEliminated: /^ *var bar =/m.test(source),
    exportThatUsesBarIsExported: /^ *exports\["exportThatUsesBar"\] =/m.test(source),
};
