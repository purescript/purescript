/* global exports */
"use strict";

// module Prelude

exports.cons = function(e) {
  return function(l) {
    return [e].concat(l);
  };
};

exports.concat = function(l1) {
  return function(l2) {
    return l1.concat(l2);
  };
};

exports.length = function(a) {
  return a.length;
}; 

exports.concatString = function(s1) {
  return function(s2) {
    return s1 + s2;
  };
};

exports.numAdd = function (n1) {
  return function (n2) {
    return n1 + n2;
  };
};

exports.numMul = function (n1) {
  return function (n2) {
    return n1 * n2;
  };
};

exports.numDiv = function (n1) {
  return function (n2) {
    return n1 / n2;
  };
};

exports.numSub = function (n1) {
  return function (n2) {
    return n1 - n2;
  };
};

exports.jsMod = function(x) {
  return function (y) {
    return x % y;
  };
};

exports.refEq = function(r1) {
  return function(r2) {
    return r1 === r2;
  };
};

exports.refIneq = function(r1) {
  return function(r2) {
    return r1 !== r2;
  };
};

exports.eqArrayImpl = function(f) {
  return function(xs) {
    return function(ys) {
      if (xs.length !== ys.length) return false;
      for (var i = 0; i < xs.length; i++) {
        if (!f(xs[i])(ys[i])) return false;
      }
      return true;
    };
  };
};

exports.ordArrayImpl = function(f) {
  return function (xs) {
    return function (ys) {
      var i = 0;
      while (i < xs.length && i < ys.length) {
        var x = xs[i];
        var y = ys[i];
        var o = f(x)(y);
        if (o !== 0) {
          return o;
        }
      }
      if (xs.length == ys.length) {
        return 0;
      } else if (xs.length > ys.length) {
        return 1;
      } else {
        return -1;
      }
    };
  };
};

exports.unsafeCompareImpl = function(lt) {
  return function(eq) {
    return function(gt) {
      return function(x) {
        return function(y) {
          return x < y ? lt : x > y ? gt : eq;
        };
      };
    };
  };
};

exports.boolOr = function (b1) {
  return function (b2) {
    return b1 || b2;
  };
};

exports.boolAnd = function (b1) {
  return function (b2) {
    return b1 && b2;
  };
};

exports.boolNot = function (b) {
  return !b;
};

exports.showNumberImpl = function (n) {
  /* jshint bitwise: false */
  return n === (n | 0) ? n + ".0" : n.toString();
};

exports.showStringImpl = function (s) {
  return JSON.stringify(s);
};

exports.showArrayImpl = function (f) {
  return function (xs) {
    var ss = [];
    for (var i = 0, l = xs.length; i < l; i++) {
      ss[i] = f(xs[i]);
    }
    return "[" + ss.join(",") + "]";
  };
};