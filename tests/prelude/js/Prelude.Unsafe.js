/* global exports */
"use strict";

// module Prelude.Unsafe

exports.unsafeIndex = function(xs) {
  return function(n) {
    return xs[n];
  };
};

exports.reflectParameterName = function(func) {
  var fnStr = func.toString();
  fnStr = fnStr.slice(fnStr.indexOf('function'));
  return fnStr.slice(fnStr.indexOf('(') + 1,fnStr.indexOf(')'));
}
