var mySTFn2 = function (a, b) {
    return a + b | 0;
};
var mySTFn1 = function (a) {
    return a + 1 | 0;
};
var myInt2 = function () {
    return mySTFn2(0, 1);
};
var myInt1 = function () {
    return mySTFn1(0);
};
export {
    mySTFn1,
    mySTFn2,
    myInt1,
    myInt2
};
