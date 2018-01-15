'use strict';

var Block  = require("bs-platform/lib/js/block.js");
var Curry  = require("bs-platform/lib/js/curry.js");
var Crypto = require("crypto");

function randomBytes(size, callback) {
  Crypto.randomBytes(size, (function (optExn, buffer) {
          return Curry._1(callback, optExn !== null ? /* Error */Block.__(1, [optExn]) : /* Ok */Block.__(0, [buffer]));
        }));
  return /* () */0;
}

exports.randomBytes = randomBytes;
/* crypto Not a pure module */
