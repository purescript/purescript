"use strict";

exports.merge = function (dict) {
  return function (l) {
    return function (r) {
      var o = {};
      return Object.assign(o, r, l);
    };
  };
};
