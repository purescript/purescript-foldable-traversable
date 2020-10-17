"use strict";

exports.arrayFrom1UpTo = function (n) {
  var result = [];
  for (var i = 1; i <= n; i++) {
    result.push(i);
  }
  return result;
};

exports.arrayReplicate = function (n) {
  return function (x) {
    var result = [];
    for (var i = 1; i <= n; i++) {
      result.push(x);
    }
    return result;
  };
};

exports.mkNEArray = function (nothing) {
  return function (just) {
    return function (arr) {
      return arr.length > 0 ? just(arr) : nothing;
    };
  };
};

exports.foldMap1NEArray = function (append) {
  return function (f) {
    return function (arr) {
      var acc = f(arr[0]);
      var len = arr.length;
      for (var i = 1; i < len; i++) {
        acc = append(acc)(f(arr[i]));
      }
      return acc;
    };
  };
};
