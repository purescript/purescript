/* global exports */
"use strict";

// module Data.Either.Unsafe

exports.unsafeThrow = function (msg) {
  throw new Error(msg);
};
