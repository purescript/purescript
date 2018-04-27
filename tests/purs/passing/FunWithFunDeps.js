
//: forall e. FVect Z e
exports.fnil = [];

//: forall n e. e -> FVect n e -> FVect (S n) e
exports.fcons = function (hd) {
  return function (tl) {
    return [hd].concat(tl);
  };
};

exports.fappend = function (dict) {
  return function (left) {
    return function (right) {
      return left.concat(right);
    };
  };
};

exports.fflatten = function (dict) {
  return function (v) {
    var accRef = [];
    for (var indexRef = 0; indexRef < v.length; indexRef += 1) {
      accRef = accRef.concat(v[indexRef]);
    }
    return accRef;
  };
};

exports.ftoArray = function (vect) {
  return vect;
};
