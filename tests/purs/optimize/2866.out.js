
// Canonical test for #2866. This doesn't need to test whether `apply`s
// defined from modules other than `Data.Function` are incorrectly
// optimized since the rest of the test suite seemingly catches it.
"use strict";
var Area = function (x) {
    return x;
};
var areaFlipped = 42;
var area = 42;
module.exports = {
    Area: Area,
    area: area,
    areaFlipped: areaFlipped
};
