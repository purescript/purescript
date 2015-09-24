// Generated by psc version 0.7.4.1
"use strict";
var $foreign = require("./foreign");
var Prelude = require("Prelude");
var Control_Monad_Eff = require("Control.Monad.Eff");
var Data_Int = require("Data.Int");
var randomRange = function (min) {
    return function (max) {
        return function __do() {
            var _20 = $foreign.random();
            return Prelude["return"](Control_Monad_Eff.applicativeEff)(_20 * (max - min) + min)();
        };
    };
};
var randomInt = function (low) {
    return function (high) {
        return function __do() {
            var _19 = $foreign.random();
            return (function () {
                var asNumber = ((Data_Int.toNumber(high) - Data_Int.toNumber(low)) + 1) * _19 + Data_Int.toNumber(low);
                return Prelude["return"](Control_Monad_Eff.applicativeEff)(Data_Int.floor(asNumber));
            })()();
        };
    };
};
var randomBool = Prelude["<$>"](Control_Monad_Eff.functorEff)(function (_2) {
    return _2 < 0.5;
})($foreign.random);
module.exports = {
    randomBool: randomBool, 
    randomRange: randomRange, 
    randomInt: randomInt, 
    random: $foreign.random
};
