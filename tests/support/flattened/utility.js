'use strict';

// module Main

exports.same = function(a) {
    return function(b) {
        return function() {
            return a === b;
        };
    };
};

exports.exit = function() {
    process.exit(1);
};
