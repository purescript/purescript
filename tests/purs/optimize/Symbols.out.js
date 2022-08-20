import * as Data_Symbol from "../Data.Symbol/index.js";
import * as Record_Unsafe from "../Record.Unsafe/index.js";
import * as Type_Proxy from "../Type.Proxy/index.js";
var fooIsSymbol = {
    reflectSymbol: function () {
        return "foo";
    }
};
var set = function (dictIsSymbol) {
    var reflectSymbol = Data_Symbol.reflectSymbol(dictIsSymbol);
    return function () {
        return function (l) {
            return function (a) {
                return function (r) {
                    return Record_Unsafe.unsafeSet(reflectSymbol(l))(a)(r);
                };
            };
        };
    };
};
var set1 = /* #__PURE__ */ set(fooIsSymbol)();
var get = function (dictIsSymbol) {
    var reflectSymbol = Data_Symbol.reflectSymbol(dictIsSymbol);
    return function () {
        return function (l) {
            return function (r) {
                return Record_Unsafe.unsafeGet(reflectSymbol(l))(r);
            };
        };
    };
};
var get1 = /* #__PURE__ */ get(fooIsSymbol)();
var get2 = /* #__PURE__ */ get({
    reflectSymbol: function () {
        return "bar";
    }
})();
var foo = /* #__PURE__ */ (function () {
    return Type_Proxy["Proxy"].value;
})();
var h = function (n) {
    return set1(foo)(n)({
        foo: 0
    });
};
var f = function (n) {
    return get1(foo)({
        foo: n
    });
};
var bar = /* #__PURE__ */ (function () {
    return Type_Proxy["Proxy"].value;
})();
var g = function (n) {
    return get2(bar)({
        foo: 0,
        bar: n
    });
};
export {
    get,
    set,
    foo,
    bar,
    f,
    g,
    h
};
