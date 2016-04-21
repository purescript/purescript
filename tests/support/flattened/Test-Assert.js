"use strict";

// module Test.Assert

exports["assert'"] = function (message) {
  return function (success) {
    return function () {
      if (!success) throw new Error(message);
      return {};
    };
  };
};

exports.checkThrows = function (fn) {
  return function () {
    try {
      fn();
      return false;
    } catch (e) {
      if (e instanceof Error) return true;
      var err = new Error("Threw something other than an Error");
      err.something = e;
      throw err;
    }
  };
};
