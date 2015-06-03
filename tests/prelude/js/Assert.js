/* global exports */
"use strict";

// module Assert

exports.error = function(msg) {
  throw msg;
};

exports.assertPartial = function(f) {
  return function() {
    try {
      return f();
    } catch (e) {
      if (e instanceof Error) return;
      throw new Error('Pattern match failure is not Error');
    }
  };
};