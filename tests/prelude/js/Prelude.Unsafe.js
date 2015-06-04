/* global exports */
"use strict";

// module Prelude.Unsafe

exports.unsafeIndex = function(xs) {
  return function(n) {
    return xs[n];
  };
};