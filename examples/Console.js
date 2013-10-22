// data IO a = \ -> a

// ret :: forall a. a -> IO a
var ret = function(a) {
    return function() { return a; };
}

// (>>=) :: forall a b. IO a -> (a -> IO b) -> IO b
var $62$62$61 = function(first) {
    return function (f) {
        return function () {
            var a = first();
            return f(a)();
        };
    };
};

// putStrLn :: String -> IO {}
var putStrLn = function(str) { return function() { console.log(str); return {}; }; };
