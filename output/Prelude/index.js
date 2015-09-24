// Generated by psc version 0.7.4.1
"use strict";
var $foreign = require("./foreign");
var Unit = function (x) {
    return x;
};
var LT = (function () {
    function LT() {

    };
    LT.value = new LT();
    return LT;
})();
var GT = (function () {
    function GT() {

    };
    GT.value = new GT();
    return GT;
})();
var EQ = (function () {
    function EQ() {

    };
    EQ.value = new EQ();
    return EQ;
})();
var Semigroupoid = function (compose) {
    this.compose = compose;
};
var Category = function (__superclass_Prelude$dotSemigroupoid_0, id) {
    this["__superclass_Prelude.Semigroupoid_0"] = __superclass_Prelude$dotSemigroupoid_0;
    this.id = id;
};
var Functor = function (map) {
    this.map = map;
};
var Apply = function (__superclass_Prelude$dotFunctor_0, apply) {
    this["__superclass_Prelude.Functor_0"] = __superclass_Prelude$dotFunctor_0;
    this.apply = apply;
};
var Applicative = function (__superclass_Prelude$dotApply_0, pure) {
    this["__superclass_Prelude.Apply_0"] = __superclass_Prelude$dotApply_0;
    this.pure = pure;
};
var Bind = function (__superclass_Prelude$dotApply_0, bind) {
    this["__superclass_Prelude.Apply_0"] = __superclass_Prelude$dotApply_0;
    this.bind = bind;
};
var Monad = function (__superclass_Prelude$dotApplicative_0, __superclass_Prelude$dotBind_1) {
    this["__superclass_Prelude.Applicative_0"] = __superclass_Prelude$dotApplicative_0;
    this["__superclass_Prelude.Bind_1"] = __superclass_Prelude$dotBind_1;
};
var Semigroup = function (append) {
    this.append = append;
};
var Semiring = function (add, mul, one, zero) {
    this.add = add;
    this.mul = mul;
    this.one = one;
    this.zero = zero;
};
var Ring = function (__superclass_Prelude$dotSemiring_0, sub) {
    this["__superclass_Prelude.Semiring_0"] = __superclass_Prelude$dotSemiring_0;
    this.sub = sub;
};
var ModuloSemiring = function (__superclass_Prelude$dotSemiring_0, div, mod) {
    this["__superclass_Prelude.Semiring_0"] = __superclass_Prelude$dotSemiring_0;
    this.div = div;
    this.mod = mod;
};
var DivisionRing = function (__superclass_Prelude$dotModuloSemiring_1, __superclass_Prelude$dotRing_0) {
    this["__superclass_Prelude.ModuloSemiring_1"] = __superclass_Prelude$dotModuloSemiring_1;
    this["__superclass_Prelude.Ring_0"] = __superclass_Prelude$dotRing_0;
};
var Num = function (__superclass_Prelude$dotDivisionRing_0) {
    this["__superclass_Prelude.DivisionRing_0"] = __superclass_Prelude$dotDivisionRing_0;
};
var Eq = function (eq) {
    this.eq = eq;
};
var Ord = function (__superclass_Prelude$dotEq_0, compare) {
    this["__superclass_Prelude.Eq_0"] = __superclass_Prelude$dotEq_0;
    this.compare = compare;
};
var Bounded = function (bottom, top) {
    this.bottom = bottom;
    this.top = top;
};
var BoundedOrd = function (__superclass_Prelude$dotBounded_0, __superclass_Prelude$dotOrd_1) {
    this["__superclass_Prelude.Bounded_0"] = __superclass_Prelude$dotBounded_0;
    this["__superclass_Prelude.Ord_1"] = __superclass_Prelude$dotOrd_1;
};
var BooleanAlgebra = function (__superclass_Prelude$dotBounded_0, conj, disj, not) {
    this["__superclass_Prelude.Bounded_0"] = __superclass_Prelude$dotBounded_0;
    this.conj = conj;
    this.disj = disj;
    this.not = not;
};
var Show = function (show) {
    this.show = show;
};
var $dollar = function (f) {
    return function (x) {
        return f(x);
    };
};
var $hash = function (x) {
    return function (f) {
        return f(x);
    };
};
var zero = function (dict) {
    return dict.zero;
};
var unsafeCompare = $foreign.unsafeCompareImpl(LT.value)(EQ.value)(GT.value);
var unit = {};
var top = function (dict) {
    return dict.top;
};
var sub = function (dict) {
    return dict.sub;
};
var $minus = function (__dict_Ring_0) {
    return sub(__dict_Ring_0);
};
var showUnit = new Show(function (_117) {
    return "unit";
});
var showString = new Show($foreign.showStringImpl);
var showOrdering = new Show(function (_118) {
    if (_118 instanceof LT) {
        return "LT";
    };
    if (_118 instanceof GT) {
        return "GT";
    };
    if (_118 instanceof EQ) {
        return "EQ";
    };
    throw new Error("Failed pattern match at Prelude line 860, column 1 - line 865, column 1: " + [ _118.constructor.name ]);
});
var showNumber = new Show($foreign.showNumberImpl);
var showInt = new Show($foreign.showIntImpl);
var showChar = new Show($foreign.showCharImpl);
var showBoolean = new Show(function (_116) {
    if (_116) {
        return "true";
    };
    if (!_116) {
        return "false";
    };
    throw new Error("Failed pattern match at Prelude line 838, column 1 - line 842, column 1: " + [ _116.constructor.name ]);
});
var show = function (dict) {
    return dict.show;
};
var showArray = function (__dict_Show_1) {
    return new Show($foreign.showArrayImpl(show(__dict_Show_1)));
};
var semiringUnit = new Semiring(function (_89) {
    return function (_90) {
        return unit;
    };
}, function (_91) {
    return function (_92) {
        return unit;
    };
}, unit, unit);
var semiringNumber = new Semiring($foreign.numAdd, $foreign.numMul, 1.0, 0.0);
var semiringInt = new Semiring($foreign.intAdd, $foreign.intMul, 1, 0);
var semigroupoidFn = new Semigroupoid(function (f) {
    return function (g) {
        return function (x) {
            return f(g(x));
        };
    };
});
var semigroupUnit = new Semigroup(function (_86) {
    return function (_87) {
        return unit;
    };
});
var semigroupString = new Semigroup($foreign.concatString);
var semigroupOrdering = new Semigroup(function (_88) {
    return function (y) {
        if (_88 instanceof LT) {
            return LT.value;
        };
        if (_88 instanceof GT) {
            return GT.value;
        };
        if (_88 instanceof EQ) {
            return y;
        };
        throw new Error("Failed pattern match at Prelude line 413, column 1 - line 418, column 1: " + [ _88.constructor.name, y.constructor.name ]);
    };
});
var semigroupArray = new Semigroup($foreign.concatArray);
var ringUnit = new Ring(function () {
    return semiringUnit;
}, function (_93) {
    return function (_94) {
        return unit;
    };
});
var ringNumber = new Ring(function () {
    return semiringNumber;
}, $foreign.numSub);
var ringInt = new Ring(function () {
    return semiringInt;
}, $foreign.intSub);
var pure = function (dict) {
    return dict.pure;
};
var $$return = function (__dict_Applicative_2) {
    return pure(__dict_Applicative_2);
};
var otherwise = true;
var one = function (dict) {
    return dict.one;
};
var not = function (dict) {
    return dict.not;
};
var negate = function (__dict_Ring_3) {
    return function (a) {
        return $minus(__dict_Ring_3)(zero(__dict_Ring_3["__superclass_Prelude.Semiring_0"]()))(a);
    };
};
var mul = function (dict) {
    return dict.mul;
};
var $times = function (__dict_Semiring_4) {
    return mul(__dict_Semiring_4);
};
var moduloSemiringUnit = new ModuloSemiring(function () {
    return semiringUnit;
}, function (_97) {
    return function (_98) {
        return unit;
    };
}, function (_99) {
    return function (_100) {
        return unit;
    };
});
var moduloSemiringNumber = new ModuloSemiring(function () {
    return semiringNumber;
}, $foreign.numDiv, function (_95) {
    return function (_96) {
        return 0.0;
    };
});
var moduloSemiringInt = new ModuloSemiring(function () {
    return semiringInt;
}, $foreign.intDiv, $foreign.intMod);
var mod = function (dict) {
    return dict.mod;
};
var map = function (dict) {
    return dict.map;
};
var $less$dollar$greater = function (__dict_Functor_5) {
    return map(__dict_Functor_5);
};
var $less$hash$greater = function (__dict_Functor_6) {
    return function (fa) {
        return function (f) {
            return $less$dollar$greater(__dict_Functor_6)(f)(fa);
        };
    };
};
var id = function (dict) {
    return dict.id;
};
var functorArray = new Functor($foreign.arrayMap);
var flip = function (f) {
    return function (b) {
        return function (a) {
            return f(a)(b);
        };
    };
};
var eqUnit = new Eq(function (_101) {
    return function (_102) {
        return true;
    };
});
var ordUnit = new Ord(function () {
    return eqUnit;
}, function (_105) {
    return function (_106) {
        return EQ.value;
    };
});
var eqString = new Eq($foreign.refEq);
var ordString = new Ord(function () {
    return eqString;
}, unsafeCompare);
var eqOrdering = new Eq(function (_103) {
    return function (_104) {
        if (_103 instanceof LT && _104 instanceof LT) {
            return true;
        };
        if (_103 instanceof GT && _104 instanceof GT) {
            return true;
        };
        if (_103 instanceof EQ && _104 instanceof EQ) {
            return true;
        };
        return false;
    };
});
var ordOrdering = new Ord(function () {
    return eqOrdering;
}, function (_107) {
    return function (_108) {
        if (_107 instanceof LT && _108 instanceof LT) {
            return EQ.value;
        };
        if (_107 instanceof EQ && _108 instanceof EQ) {
            return EQ.value;
        };
        if (_107 instanceof GT && _108 instanceof GT) {
            return EQ.value;
        };
        if (_107 instanceof LT) {
            return LT.value;
        };
        if (_107 instanceof EQ && _108 instanceof LT) {
            return GT.value;
        };
        if (_107 instanceof EQ && _108 instanceof GT) {
            return LT.value;
        };
        if (_107 instanceof GT) {
            return GT.value;
        };
        throw new Error("Failed pattern match at Prelude line 668, column 1 - line 677, column 1: " + [ _107.constructor.name, _108.constructor.name ]);
    };
});
var eqNumber = new Eq($foreign.refEq);
var ordNumber = new Ord(function () {
    return eqNumber;
}, unsafeCompare);
var eqInt = new Eq($foreign.refEq);
var ordInt = new Ord(function () {
    return eqInt;
}, unsafeCompare);
var eqChar = new Eq($foreign.refEq);
var ordChar = new Ord(function () {
    return eqChar;
}, unsafeCompare);
var eqBoolean = new Eq($foreign.refEq);
var ordBoolean = new Ord(function () {
    return eqBoolean;
}, unsafeCompare);
var eq = function (dict) {
    return dict.eq;
};
var $eq$eq = function (__dict_Eq_7) {
    return eq(__dict_Eq_7);
};
var eqArray = function (__dict_Eq_8) {
    return new Eq($foreign.eqArrayImpl($eq$eq(__dict_Eq_8)));
};
var divisionRingUnit = new DivisionRing(function () {
    return moduloSemiringUnit;
}, function () {
    return ringUnit;
});
var numUnit = new Num(function () {
    return divisionRingUnit;
});
var divisionRingNumber = new DivisionRing(function () {
    return moduloSemiringNumber;
}, function () {
    return ringNumber;
});
var numNumber = new Num(function () {
    return divisionRingNumber;
});
var div = function (dict) {
    return dict.div;
};
var $div = function (__dict_ModuloSemiring_10) {
    return div(__dict_ModuloSemiring_10);
};
var disj = function (dict) {
    return dict.disj;
};
var $bar$bar = function (__dict_BooleanAlgebra_11) {
    return disj(__dict_BooleanAlgebra_11);
};
var $$const = function (a) {
    return function (_84) {
        return a;
    };
};
var $$void = function (__dict_Functor_12) {
    return function (fa) {
        return $less$dollar$greater(__dict_Functor_12)($$const(unit))(fa);
    };
};
var conj = function (dict) {
    return dict.conj;
};
var $amp$amp = function (__dict_BooleanAlgebra_13) {
    return conj(__dict_BooleanAlgebra_13);
};
var compose = function (dict) {
    return dict.compose;
};
var functorFn = new Functor(compose(semigroupoidFn));
var $less$less$less = function (__dict_Semigroupoid_14) {
    return compose(__dict_Semigroupoid_14);
};
var $greater$greater$greater = function (__dict_Semigroupoid_15) {
    return flip(compose(__dict_Semigroupoid_15));
};
var compare = function (dict) {
    return dict.compare;
};
var ordArray = function (__dict_Ord_16) {
    return new Ord(function () {
        return eqArray(__dict_Ord_16["__superclass_Prelude.Eq_0"]());
    }, function (xs) {
        return function (ys) {
            return $dollar(compare(ordInt)(0))($foreign.ordArrayImpl(function (x) {
                return function (y) {
                    var _743 = compare(__dict_Ord_16)(x)(y);
                    if (_743 instanceof EQ) {
                        return 0;
                    };
                    if (_743 instanceof LT) {
                        return 1;
                    };
                    if (_743 instanceof GT) {
                        return negate(ringInt)(1);
                    };
                    throw new Error("Failed pattern match at Prelude line 660, column 1 - line 666, column 1: " + [ _743.constructor.name ]);
                };
            })(xs)(ys));
        };
    });
};
var $less = function (__dict_Ord_17) {
    return function (a1) {
        return function (a2) {
            var _744 = compare(__dict_Ord_17)(a1)(a2);
            if (_744 instanceof LT) {
                return true;
            };
            return false;
        };
    };
};
var $less$eq = function (__dict_Ord_18) {
    return function (a1) {
        return function (a2) {
            var _745 = compare(__dict_Ord_18)(a1)(a2);
            if (_745 instanceof GT) {
                return false;
            };
            return true;
        };
    };
};
var $greater = function (__dict_Ord_19) {
    return function (a1) {
        return function (a2) {
            var _746 = compare(__dict_Ord_19)(a1)(a2);
            if (_746 instanceof GT) {
                return true;
            };
            return false;
        };
    };
};
var $greater$eq = function (__dict_Ord_20) {
    return function (a1) {
        return function (a2) {
            var _747 = compare(__dict_Ord_20)(a1)(a2);
            if (_747 instanceof LT) {
                return false;
            };
            return true;
        };
    };
};
var categoryFn = new Category(function () {
    return semigroupoidFn;
}, function (x) {
    return x;
});
var boundedUnit = new Bounded(unit, unit);
var boundedOrdering = new Bounded(LT.value, GT.value);
var boundedOrdUnit = new BoundedOrd(function () {
    return boundedUnit;
}, function () {
    return ordUnit;
});
var boundedOrdOrdering = new BoundedOrd(function () {
    return boundedOrdering;
}, function () {
    return ordOrdering;
});
var boundedInt = new Bounded(negate(ringInt)(2147483648), 2147483647);
var boundedOrdInt = new BoundedOrd(function () {
    return boundedInt;
}, function () {
    return ordInt;
});
var boundedChar = new Bounded($foreign.bottomChar, $foreign.topChar);
var boundedOrdChar = new BoundedOrd(function () {
    return boundedChar;
}, function () {
    return ordChar;
});
var boundedBoolean = new Bounded(false, true);
var boundedOrdBoolean = new BoundedOrd(function () {
    return boundedBoolean;
}, function () {
    return ordBoolean;
});
var bottom = function (dict) {
    return dict.bottom;
};
var boundedFn = function (__dict_Bounded_21) {
    return new Bounded(function (_110) {
        return bottom(__dict_Bounded_21);
    }, function (_109) {
        return top(__dict_Bounded_21);
    });
};
var booleanAlgebraUnit = new BooleanAlgebra(function () {
    return boundedUnit;
}, function (_111) {
    return function (_112) {
        return unit;
    };
}, function (_113) {
    return function (_114) {
        return unit;
    };
}, function (_115) {
    return unit;
});
var booleanAlgebraFn = function (__dict_BooleanAlgebra_22) {
    return new BooleanAlgebra(function () {
        return boundedFn(__dict_BooleanAlgebra_22["__superclass_Prelude.Bounded_0"]());
    }, function (fx) {
        return function (fy) {
            return function (a) {
                return conj(__dict_BooleanAlgebra_22)(fx(a))(fy(a));
            };
        };
    }, function (fx) {
        return function (fy) {
            return function (a) {
                return disj(__dict_BooleanAlgebra_22)(fx(a))(fy(a));
            };
        };
    }, function (fx) {
        return function (a) {
            return not(__dict_BooleanAlgebra_22)(fx(a));
        };
    });
};
var booleanAlgebraBoolean = new BooleanAlgebra(function () {
    return boundedBoolean;
}, $foreign.boolAnd, $foreign.boolOr, $foreign.boolNot);
var $div$eq = function (__dict_Eq_9) {
    return function (x) {
        return function (y) {
            return not(booleanAlgebraBoolean)($eq$eq(__dict_Eq_9)(x)(y));
        };
    };
};
var bind = function (dict) {
    return dict.bind;
};
var liftM1 = function (__dict_Monad_23) {
    return function (f) {
        return function (a) {
            return bind(__dict_Monad_23["__superclass_Prelude.Bind_1"]())(a)(function (_16) {
                return $$return(__dict_Monad_23["__superclass_Prelude.Applicative_0"]())(f(_16));
            });
        };
    };
};
var $greater$greater$eq = function (__dict_Bind_24) {
    return bind(__dict_Bind_24);
};
var asTypeOf = function (x) {
    return function (_85) {
        return x;
    };
};
var applyFn = new Apply(function () {
    return functorFn;
}, function (f) {
    return function (g) {
        return function (x) {
            return f(x)(g(x));
        };
    };
});
var bindFn = new Bind(function () {
    return applyFn;
}, function (m) {
    return function (f) {
        return function (x) {
            return f(m(x))(x);
        };
    };
});
var apply = function (dict) {
    return dict.apply;
};
var $less$times$greater = function (__dict_Apply_25) {
    return apply(__dict_Apply_25);
};
var liftA1 = function (__dict_Applicative_26) {
    return function (f) {
        return function (a) {
            return $less$times$greater(__dict_Applicative_26["__superclass_Prelude.Apply_0"]())(pure(__dict_Applicative_26)(f))(a);
        };
    };
};
var applicativeFn = new Applicative(function () {
    return applyFn;
}, $$const);
var monadFn = new Monad(function () {
    return applicativeFn;
}, function () {
    return bindFn;
});
var append = function (dict) {
    return dict.append;
};
var $plus$plus = function (__dict_Semigroup_27) {
    return append(__dict_Semigroup_27);
};
var $less$greater = function (__dict_Semigroup_28) {
    return append(__dict_Semigroup_28);
};
var semigroupFn = function (__dict_Semigroup_29) {
    return new Semigroup(function (f) {
        return function (g) {
            return function (x) {
                return $less$greater(__dict_Semigroup_29)(f(x))(g(x));
            };
        };
    });
};
var ap = function (__dict_Monad_30) {
    return function (f) {
        return function (a) {
            return bind(__dict_Monad_30["__superclass_Prelude.Bind_1"]())(f)(function (_18) {
                return bind(__dict_Monad_30["__superclass_Prelude.Bind_1"]())(a)(function (_17) {
                    return $$return(__dict_Monad_30["__superclass_Prelude.Applicative_0"]())(_18(_17));
                });
            });
        };
    };
};
var monadArray = new Monad(function () {
    return applicativeArray;
}, function () {
    return bindArray;
});
var bindArray = new Bind(function () {
    return applyArray;
}, $foreign.arrayBind);
var applyArray = new Apply(function () {
    return functorArray;
}, ap(monadArray));
var applicativeArray = new Applicative(function () {
    return applyArray;
}, function (x) {
    return [ x ];
});
var add = function (dict) {
    return dict.add;
};
var $plus = function (__dict_Semiring_31) {
    return add(__dict_Semiring_31);
};
module.exports = {
    LT: LT, 
    GT: GT, 
    EQ: EQ, 
    Show: Show, 
    BooleanAlgebra: BooleanAlgebra, 
    BoundedOrd: BoundedOrd, 
    Bounded: Bounded, 
    Ord: Ord, 
    Eq: Eq, 
    DivisionRing: DivisionRing, 
    Num: Num, 
    Ring: Ring, 
    ModuloSemiring: ModuloSemiring, 
    Semiring: Semiring, 
    Semigroup: Semigroup, 
    Monad: Monad, 
    Bind: Bind, 
    Applicative: Applicative, 
    Apply: Apply, 
    Functor: Functor, 
    Category: Category, 
    Semigroupoid: Semigroupoid, 
    show: show, 
    "||": $bar$bar, 
    "&&": $amp$amp, 
    not: not, 
    disj: disj, 
    conj: conj, 
    bottom: bottom, 
    top: top, 
    unsafeCompare: unsafeCompare, 
    ">=": $greater$eq, 
    "<=": $less$eq, 
    ">": $greater, 
    "<": $less, 
    compare: compare, 
    "/=": $div$eq, 
    "==": $eq$eq, 
    eq: eq, 
    "-": $minus, 
    negate: negate, 
    sub: sub, 
    "/": $div, 
    mod: mod, 
    div: div, 
    "*": $times, 
    "+": $plus, 
    one: one, 
    mul: mul, 
    zero: zero, 
    add: add, 
    "++": $plus$plus, 
    "<>": $less$greater, 
    append: append, 
    ap: ap, 
    liftM1: liftM1, 
    "return": $$return, 
    ">>=": $greater$greater$eq, 
    bind: bind, 
    liftA1: liftA1, 
    pure: pure, 
    "<*>": $less$times$greater, 
    apply: apply, 
    "void": $$void, 
    "<#>": $less$hash$greater, 
    "<$>": $less$dollar$greater, 
    map: map, 
    id: id, 
    ">>>": $greater$greater$greater, 
    "<<<": $less$less$less, 
    compose: compose, 
    otherwise: otherwise, 
    asTypeOf: asTypeOf, 
    "const": $$const, 
    flip: flip, 
    "#": $hash, 
    "$": $dollar, 
    unit: unit, 
    semigroupoidFn: semigroupoidFn, 
    categoryFn: categoryFn, 
    functorFn: functorFn, 
    functorArray: functorArray, 
    applyFn: applyFn, 
    applyArray: applyArray, 
    applicativeFn: applicativeFn, 
    applicativeArray: applicativeArray, 
    bindFn: bindFn, 
    bindArray: bindArray, 
    monadFn: monadFn, 
    monadArray: monadArray, 
    semigroupString: semigroupString, 
    semigroupUnit: semigroupUnit, 
    semigroupFn: semigroupFn, 
    semigroupOrdering: semigroupOrdering, 
    semigroupArray: semigroupArray, 
    semiringInt: semiringInt, 
    semiringNumber: semiringNumber, 
    semiringUnit: semiringUnit, 
    ringInt: ringInt, 
    ringNumber: ringNumber, 
    ringUnit: ringUnit, 
    moduloSemiringInt: moduloSemiringInt, 
    moduloSemiringNumber: moduloSemiringNumber, 
    moduloSemiringUnit: moduloSemiringUnit, 
    divisionRingNumber: divisionRingNumber, 
    divisionRingUnit: divisionRingUnit, 
    numNumber: numNumber, 
    numUnit: numUnit, 
    eqBoolean: eqBoolean, 
    eqInt: eqInt, 
    eqNumber: eqNumber, 
    eqChar: eqChar, 
    eqString: eqString, 
    eqUnit: eqUnit, 
    eqArray: eqArray, 
    eqOrdering: eqOrdering, 
    ordBoolean: ordBoolean, 
    ordInt: ordInt, 
    ordNumber: ordNumber, 
    ordString: ordString, 
    ordChar: ordChar, 
    ordUnit: ordUnit, 
    ordArray: ordArray, 
    ordOrdering: ordOrdering, 
    boundedBoolean: boundedBoolean, 
    boundedUnit: boundedUnit, 
    boundedOrdering: boundedOrdering, 
    boundedInt: boundedInt, 
    boundedChar: boundedChar, 
    boundedFn: boundedFn, 
    boundedOrdBoolean: boundedOrdBoolean, 
    boundedOrdUnit: boundedOrdUnit, 
    boundedOrdOrdering: boundedOrdOrdering, 
    boundedOrdInt: boundedOrdInt, 
    boundedOrdChar: boundedOrdChar, 
    booleanAlgebraBoolean: booleanAlgebraBoolean, 
    booleanAlgebraUnit: booleanAlgebraUnit, 
    booleanAlgebraFn: booleanAlgebraFn, 
    showBoolean: showBoolean, 
    showInt: showInt, 
    showNumber: showNumber, 
    showChar: showChar, 
    showString: showString, 
    showUnit: showUnit, 
    showArray: showArray, 
    showOrdering: showOrdering
};
