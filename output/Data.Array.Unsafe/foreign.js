/* global exports */
"use strict";

// module Data.Array.Unsafe

exports.unsafeIndex = function (xs) {
  return function (n) {
    return xs[n];
  };
};
