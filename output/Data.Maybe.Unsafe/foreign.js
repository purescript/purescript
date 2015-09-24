/* global exports */
"use strict";

// module Data.Maybe.Unsafe

exports.unsafeThrow = function (msg) {
  throw new Error(msg);
};
