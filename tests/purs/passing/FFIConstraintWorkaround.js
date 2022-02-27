"use strict";

exports.showImpl = function (showFn) {
  return function (val) {
    return showFn(val);
  };
};
