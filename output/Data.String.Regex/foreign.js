/* global exports */
"use strict";

// module Data.String.Regex

exports["showRegex'"] = function (r) {
  return "" + r;
};

exports["regex'"] = function (s1) {
  return function (s2) {
    return new RegExp(s1, s2);
  };
};

exports.source = function (r) {
  return r.source;
};

exports.flags = function (r) {
  return {
    multiline: r.multiline,
    ignoreCase: r.ignoreCase,
    global: r.global,
    sticky: !!r.sticky,
    unicode: !!r.unicode
  };
};

exports.test = function (r) {
  return function (s) {
    return r.test(s);
  };
};

exports._match = function (just) {
  return function (nothing) {
    return function (r) {
      return function (s) {
        var m = s.match(r);
        if (m == null) {
          return nothing;
        } else {
          var list = [];
          for (var i = 0; i < m.length; i++) {
            list.push(m[i] == null ? nothing : just(m[i]));
          }
          return just(list);
        }
      };
    };
  };
};

exports.replace = function (r) {
  return function (s1) {
    return function (s2) {
      return s2.replace(r, s1);
    };
  };
};

exports["replace'"] = function (r) {
  return function (f) {
    return function (s2) {
      return s2.replace(r, function (match) {
        return f(match)(Array.prototype.splice.call(arguments, 1, arguments.length - 3));
      });
    };
  };
};

exports._search = function (just) {
  return function (nothing) {
    return function (r) {
      return function (s) {
        var result = s.search(r);
        return result === -1 ? nothing : just(result);
      };
    };
  };
};

exports.split = function (r) {
  return function (s) {
    return s.split(r);
  };
};
