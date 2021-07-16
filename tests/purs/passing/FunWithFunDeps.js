
//: forall e. FVect Z e
export var fnil = [];

//: forall n e. e -> FVect n e -> FVect (S n) e
export var fcons = function (hd) {
  return function (tl) {
    return [hd].concat(tl);
  };
};

export var fappend = function (dict) {
  return function (left) {
    return function (right) {
      return left.concat(right);
    };
  };
};

export var fflatten = function (dict) {
  return function (v) {
    var accRef = [];
    for (var indexRef = 0; indexRef < v.length; indexRef += 1) {
      accRef = accRef.concat(v[indexRef]);
    }
    return accRef;
  };
};

export var ftoArray = function (vect) {
  return vect;
};
