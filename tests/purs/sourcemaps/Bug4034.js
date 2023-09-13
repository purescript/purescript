export const log = function (s) {
  return function () {
    console.log(s);
  };
};

export const warn = function (s) {
  return function () {
    console.warn(s);
  };
};

export const error = function (s) {
  return function () {
    console.error(s);
  };
};

export const info = function (s) {
  return function () {
    console.info(s);
  };
};

export const debug = function (s) {
  return function () {
    console.debug(s);
  };
};

export const time = function (s) {
  return function () {
    console.time(s);
  };
};

export const timeLog = function (s) {
  return function () {
    console.timeLog(s);
  };
};

export const timeEnd = function (s) {
  return function () {
    console.timeEnd(s);
  };
};

export const clear = function () {
  console.clear();
};
