exports.show = function (showDict) {
  return function (a) {
    return showDict.show(a);
  };
};
