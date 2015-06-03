/* global exports */
"use strict";

// module Debug.Trace

exports.trace = function(s) {
  return function() {
    console.log(s);
    return {};
  };
};