export function showImpl(showFn) {
  return function (val) {
    return showFn(val);
  };
};
