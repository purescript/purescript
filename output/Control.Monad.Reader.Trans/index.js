// Generated by psc version 0.7.4.1
"use strict";
var Prelude = require("Prelude");
var Data_Distributive = require("Data.Distributive");
var Control_Alt = require("Control.Alt");
var Control_Alternative = require("Control.Alternative");
var Control_Monad_Eff_Class = require("Control.Monad.Eff.Class");
var Control_Monad_Cont_Class = require("Control.Monad.Cont.Class");
var Control_Monad_Error_Class = require("Control.Monad.Error.Class");
var Control_Monad_Reader_Class = require("Control.Monad.Reader.Class");
var Control_Monad_Rec_Class = require("Control.Monad.Rec.Class");
var Control_Monad_State_Class = require("Control.Monad.State.Class");
var Control_Monad_Writer_Class = require("Control.Monad.Writer.Class");
var Control_Monad_Trans = require("Control.Monad.Trans");
var Control_MonadPlus = require("Control.MonadPlus");
var Control_Plus = require("Control.Plus");
var Data_Either = require("Data.Either");
var ReaderT = function (x) {
    return x;
};
var runReaderT = function (_519) {
    return _519;
};
var withReaderT = function (f) {
    return function (m) {
        return ReaderT(function (_1814) {
            return runReaderT(m)(f(_1814));
        });
    };
};
var monadTransReaderT = new Control_Monad_Trans.MonadTrans(function (__dict_Monad_1) {
    return function (_1815) {
        return ReaderT(Prelude["const"](_1815));
    };
});
var mapReaderT = function (f) {
    return function (m) {
        return ReaderT(function (_1816) {
            return f(runReaderT(m)(_1816));
        });
    };
};
var functorReaderT = function (__dict_Functor_12) {
    return new Prelude.Functor(function (f) {
        return mapReaderT(Prelude["<$>"](__dict_Functor_12)(f));
    });
};
var distributiveReaderT = function (__dict_Distributive_13) {
    return new Data_Distributive.Distributive(function () {
        return functorReaderT(__dict_Distributive_13["__superclass_Prelude.Functor_0"]());
    }, function (__dict_Functor_15) {
        return function (f) {
            return function (_1817) {
                return Data_Distributive.distribute(distributiveReaderT(__dict_Distributive_13))(__dict_Functor_15)(Prelude.map(__dict_Functor_15)(f)(_1817));
            };
        };
    }, function (__dict_Functor_14) {
        return function (a) {
            return function (e) {
                return Data_Distributive.collect(__dict_Distributive_13)(__dict_Functor_14)(Prelude.flip(runReaderT)(e))(a);
            };
        };
    });
};
var applyReaderT = function (__dict_Applicative_17) {
    return new Prelude.Apply(function () {
        return functorReaderT((__dict_Applicative_17["__superclass_Prelude.Apply_0"]())["__superclass_Prelude.Functor_0"]());
    }, function (f) {
        return function (v) {
            return function (r) {
                return Prelude["<*>"](__dict_Applicative_17["__superclass_Prelude.Apply_0"]())(runReaderT(f)(r))(runReaderT(v)(r));
            };
        };
    });
};
var bindReaderT = function (__dict_Monad_16) {
    return new Prelude.Bind(function () {
        return applyReaderT(__dict_Monad_16["__superclass_Prelude.Applicative_0"]());
    }, function (m) {
        return function (k) {
            return function (r) {
                return Prelude.bind(__dict_Monad_16["__superclass_Prelude.Bind_1"]())(runReaderT(m)(r))(function (_47) {
                    return runReaderT(k(_47))(r);
                });
            };
        };
    });
};
var applicativeReaderT = function (__dict_Applicative_18) {
    return new Prelude.Applicative(function () {
        return applyReaderT(__dict_Applicative_18);
    }, function (_1818) {
        return ReaderT(Prelude["const"](Prelude.pure(__dict_Applicative_18)(_1818)));
    });
};
var monadReaderT = function (__dict_Monad_4) {
    return new Prelude.Monad(function () {
        return applicativeReaderT(__dict_Monad_4["__superclass_Prelude.Applicative_0"]());
    }, function () {
        return bindReaderT(__dict_Monad_4);
    });
};
var monadContReaderT = function (__dict_MonadCont_9) {
    return new Control_Monad_Cont_Class.MonadCont(function () {
        return monadReaderT(__dict_MonadCont_9["__superclass_Prelude.Monad_0"]());
    }, function (f) {
        return ReaderT(function (r) {
            return Control_Monad_Cont_Class.callCC(__dict_MonadCont_9)(function (c) {
                return runReaderT(f(function (a) {
                    return ReaderT(Prelude["const"](c(a)));
                }))(r);
            });
        });
    });
};
var monadEffReader = function (__dict_MonadEff_8) {
    return new Control_Monad_Eff_Class.MonadEff(function () {
        return monadReaderT(__dict_MonadEff_8["__superclass_Prelude.Monad_0"]());
    }, function (_1819) {
        return Control_Monad_Trans.lift(monadTransReaderT)(__dict_MonadEff_8["__superclass_Prelude.Monad_0"]())(Control_Monad_Eff_Class.liftEff(__dict_MonadEff_8)(_1819));
    });
};
var monadErrorReaderT = function (__dict_MonadError_7) {
    return new Control_Monad_Error_Class.MonadError(function () {
        return monadReaderT(__dict_MonadError_7["__superclass_Prelude.Monad_0"]());
    }, function (m) {
        return function (h) {
            return ReaderT(function (r) {
                return Control_Monad_Error_Class.catchError(__dict_MonadError_7)(runReaderT(m)(r))(function (e) {
                    return runReaderT(h(e))(r);
                });
            });
        };
    }, function (e) {
        return Control_Monad_Trans.lift(monadTransReaderT)(__dict_MonadError_7["__superclass_Prelude.Monad_0"]())(Control_Monad_Error_Class.throwError(__dict_MonadError_7)(e));
    });
};
var monadReaderReaderT = function (__dict_Monad_5) {
    return new Control_Monad_Reader_Class.MonadReader(function () {
        return monadReaderT(__dict_Monad_5);
    }, Prelude["return"](__dict_Monad_5["__superclass_Prelude.Applicative_0"]()), withReaderT);
};
var monadRecReaderT = function (__dict_MonadRec_3) {
    return new Control_Monad_Rec_Class.MonadRec(function () {
        return monadReaderT(__dict_MonadRec_3["__superclass_Prelude.Monad_0"]());
    }, function (k) {
        return function (a) {
            var k$prime = function (r) {
                return function (a_1) {
                    return Prelude.bind((__dict_MonadRec_3["__superclass_Prelude.Monad_0"]())["__superclass_Prelude.Bind_1"]())(runReaderT(k(a_1))(r))(function (_48) {
                        return Prelude["return"]((__dict_MonadRec_3["__superclass_Prelude.Monad_0"]())["__superclass_Prelude.Applicative_0"]())(Data_Either.either(Data_Either.Left.create)(Data_Either.Right.create)(_48));
                    });
                };
            };
            return function (r) {
                return Control_Monad_Rec_Class.tailRecM(__dict_MonadRec_3)(k$prime(r))(a);
            };
        };
    });
};
var monadStateReaderT = function (__dict_MonadState_2) {
    return new Control_Monad_State_Class.MonadState(function () {
        return monadReaderT(__dict_MonadState_2["__superclass_Prelude.Monad_0"]());
    }, function (f) {
        return Control_Monad_Trans.lift(monadTransReaderT)(__dict_MonadState_2["__superclass_Prelude.Monad_0"]())(Control_Monad_State_Class.state(__dict_MonadState_2)(f));
    });
};
var monadWriterReaderT = function (__dict_Monad_10) {
    return function (__dict_MonadWriter_11) {
        return new Control_Monad_Writer_Class.MonadWriter(function () {
            return monadReaderT(__dict_Monad_10);
        }, mapReaderT(Control_Monad_Writer_Class.listen(__dict_MonadWriter_11)), mapReaderT(Control_Monad_Writer_Class.pass(__dict_MonadWriter_11)), function (wd) {
            return Control_Monad_Trans.lift(monadTransReaderT)(__dict_Monad_10)(Control_Monad_Writer_Class.writer(__dict_MonadWriter_11)(wd));
        });
    };
};
var altReaderT = function (__dict_Alt_20) {
    return new Control_Alt.Alt(function () {
        return functorReaderT(__dict_Alt_20["__superclass_Prelude.Functor_0"]());
    }, function (m) {
        return function (n) {
            return function (r) {
                return Control_Alt["<|>"](__dict_Alt_20)(runReaderT(m)(r))(runReaderT(n)(r));
            };
        };
    });
};
var plusReaderT = function (__dict_Plus_0) {
    return new Control_Plus.Plus(function () {
        return altReaderT(__dict_Plus_0["__superclass_Control.Alt.Alt_0"]());
    }, Prelude["const"](Control_Plus.empty(__dict_Plus_0)));
};
var alternativeReaderT = function (__dict_Alternative_19) {
    return new Control_Alternative.Alternative(function () {
        return plusReaderT(__dict_Alternative_19["__superclass_Control.Plus.Plus_1"]());
    }, function () {
        return applicativeReaderT(__dict_Alternative_19["__superclass_Prelude.Applicative_0"]());
    });
};
var monadPlusReaderT = function (__dict_MonadPlus_6) {
    return new Control_MonadPlus.MonadPlus(function () {
        return alternativeReaderT(__dict_MonadPlus_6["__superclass_Control.Alternative.Alternative_1"]());
    }, function () {
        return monadReaderT(__dict_MonadPlus_6["__superclass_Prelude.Monad_0"]());
    });
};
module.exports = {
    ReaderT: ReaderT, 
    mapReaderT: mapReaderT, 
    withReaderT: withReaderT, 
    runReaderT: runReaderT, 
    functorReaderT: functorReaderT, 
    applyReaderT: applyReaderT, 
    applicativeReaderT: applicativeReaderT, 
    altReaderT: altReaderT, 
    plusReaderT: plusReaderT, 
    alternativeReaderT: alternativeReaderT, 
    bindReaderT: bindReaderT, 
    monadReaderT: monadReaderT, 
    monadPlusReaderT: monadPlusReaderT, 
    monadTransReaderT: monadTransReaderT, 
    monadEffReader: monadEffReader, 
    monadContReaderT: monadContReaderT, 
    monadErrorReaderT: monadErrorReaderT, 
    monadReaderReaderT: monadReaderReaderT, 
    monadStateReaderT: monadStateReaderT, 
    monadWriterReaderT: monadWriterReaderT, 
    distributiveReaderT: distributiveReaderT, 
    monadRecReaderT: monadRecReaderT
};
