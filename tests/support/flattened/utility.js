'use strict';

// module Main

exports.refEq = function(a) {
    return function(b) {
        return function() {
            return a === b;
        };
    };
};
