import * as Data_Semigroup from "../Data.Semigroup/index.js";
import * as Data_Symbol from "../Data.Symbol/index.js";
import * as Type_Proxy from "../Type.Proxy/index.js";
var append = /* #__PURE__ */ Data_Semigroup.append(Data_Semigroup.semigroupArray);
var findKeysAuxNil = {
    findKeysAux: function (v) {
        return [  ];
    }
};
var findKeysAux = function (dict) {
    return dict.findKeysAux;
};
var findKeysAuxCons = function (dictIsSymbol) {
    var reflectSymbol = Data_Symbol.reflectSymbol(dictIsSymbol);
    return function (dictFindKeysAux) {
        var findKeysAux1 = findKeysAux(dictFindKeysAux);
        return {
            findKeysAux: function (v) {
                return append([ reflectSymbol(Type_Proxy["Proxy"].value) ])(findKeysAux1(Type_Proxy["Proxy"].value));
            }
        };
    };
};
var findKeysAuxCons1 = /* #__PURE__ */ findKeysAuxCons({
    reflectSymbol: function () {
        return "a";
    }
});
var findKeysAuxCons2 = /* #__PURE__ */ findKeysAuxCons({
    reflectSymbol: function () {
        return "b";
    }
});
var findKeysAuxCons3 = /* #__PURE__ */ findKeysAuxCons({
    reflectSymbol: function () {
        return "c";
    }
});
var findKeysAuxCons4 = /* #__PURE__ */ findKeysAuxCons({
    reflectSymbol: function () {
        return "d";
    }
});
var findKeys = function () {
    return function (dictFindKeysAux) {
        var findKeysAux1 = findKeysAux(dictFindKeysAux);
        return function (v) {
            return findKeysAux1(Type_Proxy["Proxy"].value);
        };
    };
};
var findKeys11 = /* #__PURE__ */ findKeys();
var findKeys12 = /* #__PURE__ */ findKeys11(/* #__PURE__ */ findKeysAuxCons1(findKeysAuxNil));
var findKeys13 = /* #__PURE__ */ findKeys11(/* #__PURE__ */ findKeysAuxCons1(/* #__PURE__ */ findKeysAuxCons2(/* #__PURE__ */ findKeysAuxCons3(/* #__PURE__ */ findKeysAuxCons4(/* #__PURE__ */ findKeysAuxCons({
    reflectSymbol: function () {
        return "e";
    }
})(findKeysAuxNil))))));
var findKeys14 = /* #__PURE__ */ findKeys11(/* #__PURE__ */ findKeysAuxCons1(/* #__PURE__ */ findKeysAuxCons2(findKeysAuxNil)));
var findKeys15 = /* #__PURE__ */ findKeys11(/* #__PURE__ */ findKeysAuxCons1(/* #__PURE__ */ findKeysAuxCons2(/* #__PURE__ */ findKeysAuxCons3(findKeysAuxNil))));
var findKeys16 = /* #__PURE__ */ findKeys11(/* #__PURE__ */ findKeysAuxCons1(/* #__PURE__ */ findKeysAuxCons2(/* #__PURE__ */ findKeysAuxCons3(/* #__PURE__ */ findKeysAuxCons4(findKeysAuxNil)))));
var findKeys1 = /* #__PURE__ */ (function () {
    return findKeys12(Type_Proxy["Proxy"].value);
})();
var findKeys10 = /* #__PURE__ */ (function () {
    return findKeys13(Type_Proxy["Proxy"].value);
})();
var findKeys2 = /* #__PURE__ */ (function () {
    return findKeys14(Type_Proxy["Proxy"].value);
})();
var findKeys3 = /* #__PURE__ */ (function () {
    return findKeys15(Type_Proxy["Proxy"].value);
})();
var findKeys4 = /* #__PURE__ */ (function () {
    return findKeys16(Type_Proxy["Proxy"].value);
})();
var findKeys5 = /* #__PURE__ */ (function () {
    return findKeys13(Type_Proxy["Proxy"].value);
})();
var findKeys6 = /* #__PURE__ */ (function () {
    return findKeys12(Type_Proxy["Proxy"].value);
})();
var findKeys7 = /* #__PURE__ */ (function () {
    return findKeys14(Type_Proxy["Proxy"].value);
})();
var findKeys8 = /* #__PURE__ */ (function () {
    return findKeys15(Type_Proxy["Proxy"].value);
})();
var findKeys9 = /* #__PURE__ */ (function () {
    return findKeys16(Type_Proxy["Proxy"].value);
})();
export {
    findKeysAux,
    findKeys,
    findKeys1,
    findKeys2,
    findKeys3,
    findKeys4,
    findKeys5,
    findKeys6,
    findKeys7,
    findKeys8,
    findKeys9,
    findKeys10,
    findKeysAuxNil,
    findKeysAuxCons
};
