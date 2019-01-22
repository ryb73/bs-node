'use strict';

var Fs = require("fs");
var Curry = require("bs-platform/lib/js/curry.js");
var Js_exn = require("bs-platform/lib/js/js_exn.js");
var Caml_exceptions = require("bs-platform/lib/js/caml_exceptions.js");

var Watch = /* module */[];

function unbox(param) {
  return param[0];
}

function _end(s) {
  s[0].end();
  return /* () */0;
}

var Exn = Caml_exceptions.create("NodeFs-BsNode.Stream.Exn");

function _onImpl(s, handler) {
  var variant = handler[0];
  if (variant !== -215364664) {
    if (variant >= 869223765) {
      s.on("open", handler[1]);
      return /* () */0;
    } else {
      s.on("close", handler[1]);
      return /* () */0;
    }
  } else {
    s.on("error", handler[1]);
    return /* () */0;
  }
}

function on(param) {
  var partial_arg = param[0];
  return (function (param) {
      return _onImpl(partial_arg, param);
    });
}

function Make(StreamType) {
  var stream = function (s) {
    return /* Stream */[s];
  };
  var on = function (s) {
    return (function (param) {
        return _onImpl(s, param);
      });
  };
  var _end = function (s) {
    s.end();
    return /* () */0;
  };
  return /* module */[
          /* stream */stream,
          /* on */on,
          /* _end */_end
        ];
}

var Stream = /* module */[
  /* unbox */unbox,
  /* _end */_end,
  /* Exn */Exn,
  /* _onImpl */_onImpl,
  /* on */on,
  /* Make */Make
];

function unbox$1(param) {
  return param[0];
}

function write(s, d) {
  s[0].write(d);
  return /* () */0;
}

function Make$1(StreamType) {
  var stream = function (s) {
    return /* Stream */[s];
  };
  var on = function (s) {
    return (function (param) {
        return _onImpl(s, param);
      });
  };
  var _end = function (s) {
    s.end();
    return /* () */0;
  };
  var writeable = function (stream) {
    return /* Writeable */[stream];
  };
  var write = function (stream, data) {
    stream.write(data);
    return /* () */0;
  };
  return /* module */[
          /* stream */stream,
          /* on */on,
          /* _end */_end,
          /* writeable */writeable,
          /* write */write
        ];
}

var Writeable = /* module */[
  /* unbox */unbox$1,
  /* write */write,
  /* Make */Make$1
];

function unbox$2(param) {
  return param[0];
}

function pipe(instream, outstream) {
  instream[0].pipe(outstream[0]);
  return /* () */0;
}

function Make$2(StreamType) {
  var stream = function (s) {
    return /* Stream */[s];
  };
  var on = function (s) {
    return (function (param) {
        return _onImpl(s, param);
      });
  };
  var _end = function (s) {
    s.end();
    return /* () */0;
  };
  var readable = function (s) {
    return /* Readable */[s];
  };
  var pipe = function (instream, outstream) {
    instream.pipe(outstream[0]);
    return /* () */0;
  };
  return /* module */[
          /* stream */stream,
          /* on */on,
          /* _end */_end,
          /* readable */readable,
          /* pipe */pipe
        ];
}

var Readable = /* module */[
  /* unbox */unbox$2,
  /* pipe */pipe,
  /* Make */Make$2
];

function stream(s) {
  return /* Stream */[s];
}

function on$1(s) {
  return (function (param) {
      return _onImpl(s, param);
    });
}

function _end$1(s) {
  s.end();
  return /* () */0;
}

var on$2 = /* on */on$1;

var _end$2 = /* _end */_end$1;

function writeable(stream) {
  return /* Writeable */[stream];
}

function write$1(stream, data) {
  stream.write(data);
  return /* () */0;
}

function create(path, options, scope) {
  return new Promise((function (resolve, reject) {
                var stream = Fs.createWriteStream(path, options);
                Curry._2(on$2, stream, /* `error */[
                      -215364664,
                      (function (err) {
                          return reject([
                                      Exn,
                                      err
                                    ]);
                        })
                    ]);
                return Curry._2(on$2, stream, /* `open_ */[
                            869223765,
                            (function (param) {
                                try {
                                  Curry._1(scope, stream).then((function (param) {
                                            resolve(Curry._1(_end$2, stream));
                                            return Promise.resolve(/* () */0);
                                          })).catch((function (err) {
                                          Promise.resolve(err);
                                          (( reject(err) ));
                                          return Promise.resolve(/* () */0);
                                        }));
                                  return /* () */0;
                                }
                                catch (raw_err){
                                  var err = Js_exn.internalToOCamlException(raw_err);
                                  return reject(err);
                                }
                              })
                          ]);
              }));
}

var FileWriteStream_000 = /* stream */stream;

var FileWriteStream = /* module */[
  FileWriteStream_000,
  /* on */on$2,
  /* _end */_end$2,
  /* writeable */writeable,
  /* write */write$1,
  /* create */create
];

function stream$1(s) {
  return /* Stream */[s];
}

function on$3(s) {
  return (function (param) {
      return _onImpl(s, param);
    });
}

function _end$3(s) {
  s.end();
  return /* () */0;
}

function readable(s) {
  return /* Readable */[s];
}

function pipe$1(instream, outstream) {
  instream.pipe(outstream[0]);
  return /* () */0;
}

var FileReadStream_000 = /* stream */stream$1;

var FileReadStream_001 = /* on */on$3;

var FileReadStream_002 = /* _end */_end$3;

var FileReadStream = /* module */[
  FileReadStream_000,
  FileReadStream_001,
  FileReadStream_002,
  /* readable */readable,
  /* pipe */pipe$1
];

exports.Watch = Watch;
exports.Stream = Stream;
exports.Writeable = Writeable;
exports.Readable = Readable;
exports.FileWriteStream = FileWriteStream;
exports.FileReadStream = FileReadStream;
/* fs Not a pure module */
