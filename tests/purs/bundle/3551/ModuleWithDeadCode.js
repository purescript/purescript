import * as fs from 'fs';

var source = fs.readFileSync(__filename, 'utf-8');

export var results = {
    fooIsNotEliminated: /^ *var foo =/m.test(source),
    barIsExported: /^ *exports\["bar"\] =/m.test(source),
    barIsNotEliminated: /^ *var bar =/m.test(source),
    exportThatUsesBarIsExported: /^ *exports\["exportThatUsesBar"\] =/m.test(source),
};
