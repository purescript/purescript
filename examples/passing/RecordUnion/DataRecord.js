exports.merge = function (dict) {
  return function (l) {
    return function (r) {
      var o = {}
      return Object.assign(o, l, r);
    }
  }
}
