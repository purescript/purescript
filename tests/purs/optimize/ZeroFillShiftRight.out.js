var shift = function (x) {
    return function (count) {
        return x >>> count | 0;
    };
};
export {
    shift
};
