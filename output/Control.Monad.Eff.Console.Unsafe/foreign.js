/* global exports, console */
"use strict";

// module Control.Monad.Eff.Console.Unsafe

exports.logAny = function (s) {
  return function () {
    console.log(s);
    return {};
  };
};

exports.errorAny = function (s) {
  return function () {
    console.error(s);
    return {};
  };
};
