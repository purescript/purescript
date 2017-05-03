"use strict";

exports.unsafeGet = function (s) {
  return function (o) {
    return o[s];
  };
};

exports.unsafeSet = function(s) {
  return function(a) {
    return function (o) {
      var o1 = {};
      o1[s] = a;
      return Object.assign({}, o, o1);
    };
  };
};
