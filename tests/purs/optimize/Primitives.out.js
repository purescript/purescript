
// This test checks that no unused Semiring abstractions are introduced when
// the operators are compiled to JS primitives.
"use strict";
var f = function (x) {
    return function (y) {
        return x * (y + 1 | 0) | 0;
    };
};
module.exports = {
    f: f
};
