"use strict";

// module Test.Main

exports.arrayFrom1UpTo = function (n) {
  var result = [];
  for (var i = 1; i <= n; i++) {
    result.push(i);
  }
  return result;
};
