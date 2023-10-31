var $runtime_lazy = function (name, moduleName, init) {
    var state = 0;
    var val;
    return function (lineNumber) {
        if (state === 2) return val;
        if (state === 1) throw new ReferenceError(name + " was needed before it finished initializing (module " + moduleName + ", line " + lineNumber + ")", moduleName, lineNumber);
        state = 1;
        val = init();
        state = 2;
        return val;
    };
};

// This is a test that TCO isn't broken by unsafePartial.
var tcoable = function ($copy_v) {
    var $tco_done = false;
    var $tco_result;
    function $tco_loop(v) {
        if (v === 0) {
            $tco_done = true;
            return "done";
        };
        if (v > 0) {
            $copy_v = v - 1 | 0;
            return;
        };
        throw new Error("Failed pattern match at Main (line 43, column 25 - line 45, column 31): " + [ v.constructor.name ]);
    };
    while (!$tco_done) {
        $tco_result = $tco_loop($copy_v);
    };
    return $tco_result;
};
var isOdd = function (n) {
    return n > 0 && !isEven(n - 1 | 0);
};
var isEven = function (n) {
    return n === 0 || isOdd(n - 1 | 0);
};

// This is an example of four mutually recursive bindings with a complex
// run-time dependency structure. The expected result is:
//   alpha is defined without any laziness
//   bravo and charlie are lazily initialized in a group
//   and then delta is lazily initialized
var alpha = function (v) {
    if (v === 0) {
        return $lazy_bravo(18);
    };
    if (v === 1) {
        return $lazy_charlie(19);
    };
    if (v === 2) {
        return function (y) {
            var $7 = y > 0;
            if ($7) {
                return bravo(y);
            };
            return charlie(y);
        };
    };
    return function (y) {
        return $lazy_delta(21)(y)(v);
    };
};
var $lazy_charlie = /* #__PURE__ */ $runtime_lazy("charlie", "Main", function () {
    return (function (v) {
        return alpha;
    })({})(4);
});
var $lazy_bravo = /* #__PURE__ */ $runtime_lazy("bravo", "Main", function () {
    return (function (v) {
        return alpha;
    })({})(3);
});
var charlie = /* #__PURE__ */ $lazy_charlie(31);
var bravo = /* #__PURE__ */ $lazy_bravo(28);
var $lazy_delta = /* #__PURE__ */ $runtime_lazy("delta", "Main", function () {
    var b = (function (v) {
        return bravo;
    })({});
    return function (x) {
        return function (y) {
            var $8 = x === y;
            if ($8) {
                return b(0);
            };
            return 1.0;
        };
    };
});
var delta = /* #__PURE__ */ $lazy_delta(34);
export {
    isEven,
    isOdd,
    alpha,
    bravo,
    charlie,
    delta,
    tcoable
};
