"use strict";
var Data_Symbol = require("../Data.Symbol/index.js");
var Record_Unsafe = require("../Record.Unsafe/index.js");
var Type_Proxy = require("../Type.Proxy/index.js");
var get = function (dictIsSymbol) {
    var reflectSymbol = Data_Symbol.reflectSymbol(dictIsSymbol);
    return function (dictCons) {
        return function (l) {
            return function (r) {
                return Record_Unsafe.unsafeGet(reflectSymbol(l))(r);
            };
        };
    };
};
var get1 = get(new Data_Symbol.IsSymbol(function () {
    return "foo";
}))();
var foo = Type_Proxy["Proxy"].value;
var g = function (n) {
    return get1(foo)({
        foo: n,
        bar: 42
    });
};
var f = function (n) {
    return get1(foo)({
        foo: n
    });
};
module.exports = {
    get: get,
    foo: foo,
    f: f,
    g: g
};
