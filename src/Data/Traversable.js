"use strict";

// jshint maxparams: 3

exports.traverseArrayImpl = function () {
  function array1(a) {return [a];}

  function array2(a) {
    return function (b) {return [a, b];};
  }

  function concat2(xs) {
    return function (ys) {return xs.concat(ys);};
  }

  return function (apply) {
    return function (map) {
      return function (pure) {
        return function (f) {
          return function (array) {
            function go(bot, top) {
              switch (top - bot) {
              case 0: return pure([]);
              case 1: return map(array1)(f(array[bot]));
              case 2: return apply(map(array2)(f(array[bot])))(f(array[bot + 1]));
              default:
                var pivot = Math.floor((top + bot) / 2);
                return apply(map(concat2)(go(bot, pivot)))(go(pivot, top));
              }
            }
            return go(0, array.length);
          };
        };
      };
    };
  };
}();
