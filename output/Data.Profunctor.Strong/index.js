// Generated by psc version 0.7.4.1
"use strict";
var Prelude = require("Prelude");
var Data_Profunctor = require("Data.Profunctor");
var Data_Tuple = require("Data.Tuple");
var Strong = function (__superclass_Data$dotProfunctor$dotProfunctor_0, first, second) {
    this["__superclass_Data.Profunctor.Profunctor_0"] = __superclass_Data$dotProfunctor$dotProfunctor_0;
    this.first = first;
    this.second = second;
};
var strongFn = new Strong(function () {
    return Data_Profunctor.profunctorFn;
}, function (a2b) {
    return function (_537) {
        return new Data_Tuple.Tuple(a2b(_537.value0), _537.value1);
    };
}, Prelude["<$>"](Data_Tuple.functorTuple));
var second = function (dict) {
    return dict.second;
};
var first = function (dict) {
    return dict.first;
};
var $times$times$times = function (__dict_Category_0) {
    return function (__dict_Strong_1) {
        return function (l) {
            return function (r) {
                return Prelude[">>>"](__dict_Category_0["__superclass_Prelude.Semigroupoid_0"]())(first(__dict_Strong_1)(l))(second(__dict_Strong_1)(r));
            };
        };
    };
};
var $amp$amp$amp = function (__dict_Category_2) {
    return function (__dict_Strong_3) {
        return function (l) {
            return function (r) {
                var split = Data_Profunctor.dimap(__dict_Strong_3["__superclass_Data.Profunctor.Profunctor_0"]())(Prelude.id(Prelude.categoryFn))(function (a) {
                    return new Data_Tuple.Tuple(a, a);
                })(Prelude.id(__dict_Category_2));
                return Prelude[">>>"](__dict_Category_2["__superclass_Prelude.Semigroupoid_0"]())(split)($times$times$times(__dict_Category_2)(__dict_Strong_3)(l)(r));
            };
        };
    };
};
module.exports = {
    Strong: Strong, 
    "&&&": $amp$amp$amp, 
    "***": $times$times$times, 
    second: second, 
    first: first, 
    strongFn: strongFn
};
