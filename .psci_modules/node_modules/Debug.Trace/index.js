"use strict";
var Prelude = require("Prelude");
function trace(s) {  return function() {    console.log(s);    return {};  };};
var print = function (__dict_Show_0) {
    return function (o) {
        return trace(Prelude.show(__dict_Show_0)(o));
    };
};
module.exports = {
    print: print, 
    trace: trace
};