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

exports.assertEqual = function(a) {
  return function(b) {
    return function() {
      if (a != b) {
        throw new Error('Assertion failed - values do not match: ' + a + ' -vs- ' + b);
      }
    };
  };
}
