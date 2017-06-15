"use strict";

exports.ifoldrArray = function (f) {
  return function (init) {
    return function (xs) {
      var acc = init;
      var len = xs.length;
      for (var i = len - 1; i >= 0; i--) {
        acc = f(i)(xs[i])(acc);
      }
      return acc;
    };
  };
};

exports.ifoldlArray = function (f) {
  return function (init) {
    return function (xs) {
      var acc = init;
      var len = xs.length;
      for (var i = 0; i < len; i++) {
        acc = f(i)(acc)(xs[i]);
      }
      return acc;
    };
  };
};
