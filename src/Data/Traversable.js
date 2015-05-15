/* global exports */
"use strict";

// module Data.Traversable

exports.traverseArrayImpl = function () {
  function Cont (fn) {
    this.fn = fn;
  }

  function consArray(x) {
    return function (xs) {
      return [x].concat(xs);
    };
  }

  return function (apply) {
    return function (map) {
      return function (pure) {
        return function (f) {
          /* jshint maxparams: 2 */
          var buildFrom = function (x, ys) {
            return apply(map(consArray)(f(x)))(ys);
          };

          /* jshint maxparams: 3 */
          var go = function (acc, currentLen, xs) {
            if (currentLen === 0) {
              return acc;
            } else {
              var last = xs[currentLen - 1];
              return new Cont(function () {
                return go(buildFrom(last, acc), currentLen - 1, xs);
              });
            }
          };

          return function (array) {
            var result = go(pure([]), array.length, array);
            while (result instanceof Cont) {
              result = result.fn();
            }

            return result;
          };
        };
      };
    };
  };
}();
