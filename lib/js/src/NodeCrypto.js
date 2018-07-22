'use strict';

var Block = require("bs-platform/lib/js/block.js");
var Curry = require("bs-platform/lib/js/curry.js");
var Crypto = require("crypto");

function appendString(hmac, str) {
  hmac.update(str);
  return /* () */0;
}

function appendBuffer(hmac, buffer) {
  hmac.update(buffer);
  return /* () */0;
}

function toBase64(hmac) {
  return hmac.digest("base64");
}

function toBuffer(hmac) {
  return hmac.digest(undefined);
}

function Hmac_000(prim, prim$1) {
  return Crypto.createHmac((function () {
                  switch (prim) {
                    case -812272505 : 
                        return "sha256";
                    case -812124214 : 
                        return "sha512";
                    case -812222109 : 
                        return "sha384";
                    case 924029093 : 
                        return "sha1";
                    case 3844350 : 
                        return "md5";
                    
                  }
                })(), prim$1);
}

var Hmac = /* module */[
  Hmac_000,
  /* appendString */appendString,
  /* appendBuffer */appendBuffer,
  /* toBase64 */toBase64,
  /* toBuffer */toBuffer
];

function appendString$1(hmac, str) {
  hmac.update(str);
  return /* () */0;
}

function appendBuffer$1(hmac, buffer) {
  hmac.update(buffer);
  return /* () */0;
}

function toBase64$1(hmac) {
  return hmac.digest("base64");
}

function toBuffer$1(hmac) {
  return hmac.digest(undefined);
}

function Hash_000(prim) {
  return Crypto.createHash((function () {
                  switch (prim) {
                    case -812272505 : 
                        return "sha256";
                    case -812124214 : 
                        return "sha512";
                    case -812222109 : 
                        return "sha384";
                    case 924029093 : 
                        return "sha1";
                    case 3844350 : 
                        return "md5";
                    
                  }
                })());
}

var Hash = /* module */[
  Hash_000,
  /* appendString */appendString$1,
  /* appendBuffer */appendBuffer$1,
  /* toBase64 */toBase64$1,
  /* toBuffer */toBuffer$1
];

function randomBytes(size, callback) {
  Crypto.randomBytes(size, (function (optExn, buffer) {
          if (optExn !== null) {
            return Curry._1(callback, /* Error */Block.__(1, [optExn]));
          } else {
            return Curry._1(callback, /* Ok */Block.__(0, [buffer]));
          }
        }));
  return /* () */0;
}

exports.Hmac = Hmac;
exports.Hash = Hash;
exports.randomBytes = randomBytes;
/* crypto Not a pure module */
