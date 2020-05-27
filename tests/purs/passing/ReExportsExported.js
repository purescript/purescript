"use strict";

// Import `A.a` which was re-exported from `B` and then again from `C`
exports.a = require('../C').a;
