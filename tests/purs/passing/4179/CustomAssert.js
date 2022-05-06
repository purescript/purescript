export var assertThrowsImpl = function (arg) {
  return function (f) {
    return function () {
      try {
        f(arg);
      } catch (e) {
        return e.toString();
      }
      throw new Error("Assertion failed: An error should have been thrown");
    };
  };
};
