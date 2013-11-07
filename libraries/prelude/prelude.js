// Arrays

function indexOf(l) { return function (e) { return l.indexOf(e); }; }
function lastIndexOf(l) { return function (e) { return l.lastIndexOf(e); }; }
function concat(l1) { return function (l2) { return l1.concat(l2); }; }
function join(l) { return l.join(); }
function joinWith(l) { return function (s) { return l.join(s); }; }
function push(l) { return function (e) { var l1 = l.slice(); l1.push(e); return l1; }; }
function reverse(l) { var l1 = l.slice(); l1.reverse(); return l1; }
function shift(l) { var l1 = l.slice(); l1.shift(); return l1; }
function slice(s) { return function(e) { return function (l) { return l.slice(s, e); }; }; }
function sort(l) { var l1 = l.slice(); l.sort(); return l1; }
function splice(s) { return function(e) { return function(l1) { return function(l2) { return l2.splice(s, e, l1); }; }; }; }

function charAt(i) { return function(s) { return s.charAt(i); }; }
function indexOfS(s1) { return function(s2) { return s2.indexOf(s2); }; } 
function lastIndexOfS(s1) { return function(s2) { return s2.lastIndexOf(s2); }; } 
function localeCompare(s1) { return function(s2) { return s1.localeCompare(s2); }; }
function replace(s1) { return function(s2) { return function(s3) { return s3.replace(s1, s2) }; }; }
function sliceS(st) { return function(e) { return function(s) { return s.slice(st, e); }; }; }
function split(sep) { return function(s) { return s.split(s); }; }
function substr(n1) { return function(n2) { return function(s) { return s.substr(n1, n2); }; }; }
function substring(n1) { return function(n2) { return function(s) { return s.substring(n1, n2); }; }; }
function toLower(s) { return s.toLower(); }
function toUpper(s) { return s.toUpper(); }
function trim(s) { return s.trim(); }

function regex(s1) { return function(s2) { return new Regex(s1, s2); }; };
function test(r) { return function (s) { return r.test(s); }; };
function match(r) { return function (s) { return s.match(r); }; };
function replaceR(r) { return function(s1) { return function(s2) { return s2.replace(r, s1) }; }; }
function search(r) { return function (s) { return s.search(r); }; };

var nan = NaN;
var infinity = Infinity;
function toExponential(n) { return n.toExponential(); }
function toFixed(d) { return function(n) { return n.toFixed(d); }; } 
function toPrecision(d) { return function(n) { return n.toPrecision(d); }; }
function numberToString(n) { return n.toString(); }

var math = Math;

function map(f) { 
  return function(xs) {
    var ys = [];
    for (var i = 0; i < xs.length; i++) {
      ys[i] = f(xs[i]);
    }
    return ys;
  };
}

function filter(xs) {
  return function (p) {
    var ys = [];
    var j = 0;
    for (var i = 0; i < xs.length; i++) {
      var x = xs[i];
      if (p(x)) {
        ys[j++] = x;
      }
    }
    return ys;
  };
}
