"use strict";

exports.mergeImpl = function (l) {
  return function (r) {
    var o = {};
    return Object.assign(o, r, l);
  };
};
