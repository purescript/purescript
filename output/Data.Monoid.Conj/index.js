// Generated by psc version 0.7.4.1
"use strict";
var Prelude = require("Prelude");
var Control_Comonad = require("Control.Comonad");
var Control_Extend = require("Control.Extend");
var Data_Monoid = require("Data.Monoid");
var Conj = function (x) {
    return x;
};
var showConj = function (__dict_Show_0) {
    return new Prelude.Show(function (_166) {
        return "Conj (" + (Prelude.show(__dict_Show_0)(_166) + ")");
    });
};
var semiringConj = function (__dict_BooleanAlgebra_1) {
    return new Prelude.Semiring(function (_169) {
        return function (_170) {
            return Prelude.conj(__dict_BooleanAlgebra_1)(_169)(_170);
        };
    }, function (_171) {
        return function (_172) {
            return Prelude.disj(__dict_BooleanAlgebra_1)(_171)(_172);
        };
    }, Prelude.bottom(__dict_BooleanAlgebra_1["__superclass_Prelude.Bounded_0"]()), Prelude.top(__dict_BooleanAlgebra_1["__superclass_Prelude.Bounded_0"]()));
};
var semigroupConj = function (__dict_BooleanAlgebra_2) {
    return new Prelude.Semigroup(function (_167) {
        return function (_168) {
            return Prelude.conj(__dict_BooleanAlgebra_2)(_167)(_168);
        };
    });
};
var runConj = function (_157) {
    return _157;
};
var monoidConj = function (__dict_BooleanAlgebra_4) {
    return new Data_Monoid.Monoid(function () {
        return semigroupConj(__dict_BooleanAlgebra_4);
    }, Prelude.top(__dict_BooleanAlgebra_4["__superclass_Prelude.Bounded_0"]()));
};
var functorConj = new Prelude.Functor(function (f) {
    return function (_162) {
        return f(_162);
    };
});
var extendConj = new Control_Extend.Extend(function () {
    return functorConj;
}, function (f) {
    return function (x) {
        return f(x);
    };
});
var eqConj = function (__dict_Eq_5) {
    return new Prelude.Eq(function (_158) {
        return function (_159) {
            return Prelude["=="](__dict_Eq_5)(_158)(_159);
        };
    });
};
var ordConj = function (__dict_Ord_3) {
    return new Prelude.Ord(function () {
        return eqConj(__dict_Ord_3["__superclass_Prelude.Eq_0"]());
    }, function (_160) {
        return function (_161) {
            return Prelude.compare(__dict_Ord_3)(_160)(_161);
        };
    });
};
var comonadConj = new Control_Comonad.Comonad(function () {
    return extendConj;
}, runConj);
var boundedConj = function (__dict_Bounded_6) {
    return new Prelude.Bounded(Prelude.bottom(__dict_Bounded_6), Prelude.top(__dict_Bounded_6));
};
var applyConj = new Prelude.Apply(function () {
    return functorConj;
}, function (_163) {
    return function (_164) {
        return _163(_164);
    };
});
var bindConj = new Prelude.Bind(function () {
    return applyConj;
}, function (_165) {
    return function (f) {
        return f(_165);
    };
});
var applicativeConj = new Prelude.Applicative(function () {
    return applyConj;
}, Conj);
var monadConj = new Prelude.Monad(function () {
    return applicativeConj;
}, function () {
    return bindConj;
});
module.exports = {
    Conj: Conj, 
    runConj: runConj, 
    eqConj: eqConj, 
    ordConj: ordConj, 
    boundedConj: boundedConj, 
    functorConj: functorConj, 
    applyConj: applyConj, 
    applicativeConj: applicativeConj, 
    bindConj: bindConj, 
    monadConj: monadConj, 
    extendConj: extendConj, 
    comonadConj: comonadConj, 
    showConj: showConj, 
    semigroupConj: semigroupConj, 
    monoidConj: monoidConj, 
    semiringConj: semiringConj
};
