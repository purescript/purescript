/* global exports */
"use strict";

// module Data.Exists

exports.mkExists = function (fa) {
  return fa;
};

exports.runExists = function (f) {
  return function (fa) {
    return f(fa);
  };
};
