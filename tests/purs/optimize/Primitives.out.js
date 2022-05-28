// This test checks that no unused Semiring abstractions are introduced when
// the operators are compiled to JS primitives.
var f = function (x) {
    return function (y) {
        return x * (y + 1 | 0) | 0;
    };
};
export {
    f
};
