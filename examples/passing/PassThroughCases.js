'use strict';

exports.refEq = function(a) {
    return function(b) {
        return function() {
            return a === b;
        };
    };
};
