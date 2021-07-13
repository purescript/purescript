"use strict";
var Control_Applicative = require("../Control.Applicative/index.js");
var Control_Bind = require("../Control.Bind/index.js");
var Monad = function (Applicative0, Bind1) {
    this.Applicative0 = Applicative0;
    this.Bind1 = Bind1;
};
var liftM1 = function (dictMonad) {
    var bind = Control_Bind.bind(dictMonad.Bind1());
    var pure = Control_Applicative.pure(dictMonad.Applicative0());
    return function (f) {
        return function (a) {
            return bind(a)(function (a$prime) {
                return pure(f(a$prime));
            });
        };
    };
};
var ap = function (dictMonad) {
    var bind = Control_Bind.bind(dictMonad.Bind1());
    var pure = Control_Applicative.pure(dictMonad.Applicative0());
    return function (f) {
        return function (a) {
            return bind(f)(function (f$prime) {
                return bind(a)(function (a$prime) {
                    return pure(f$prime(a$prime));
                });
            });
        };
    };
};
module.exports = {
    Monad: Monad,
    liftM1: liftM1,
    ap: ap
};
