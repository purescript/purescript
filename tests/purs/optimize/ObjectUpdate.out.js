var staticUpdate2 = function (x) {
    return {
        alpha: x.alpha,
        bravo: true
    };
};
var staticUpdate1 = function (x) {
    return {
        alpha: x.alpha,
        bravo: "replaced"
    };
};
var dynamicUpdate1 = function (x) {
    var $3 = {};
    for (var $4 in x) {
        if ({}.hasOwnProperty.call(x, $4)) {
            $3[$4] = x[$4];
        };
    };
    $3.bravo = true;
    return $3;
};
export {
    staticUpdate1,
    staticUpdate2,
    dynamicUpdate1
};
