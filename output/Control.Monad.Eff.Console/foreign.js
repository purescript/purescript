/* global exports, console */
"use strict";

// module Control.Monad.Eff.Console

exports.log = function (s) {
  return function () {
    console.log(s);
    return {};
  };
};

exports.error = function (s) {
  return function () {
    console.error(s);
    return {};
  };
};
