"use strict";

var _ = [];

exports.push = (i) => {
  _.push(i);
  // _ = [i].concat(_);
  return () => {};
};
exports.pop = () => {
  const x = _.pop();
  if (x === undefined) return [];
  else return [x];
  // if (_.length === 0) {
  //   return [];
  // }
  // var __ = [];
  // var i = 0;
  // for (; i < _.length - 1; i++) {
  //   __.push(_[i]);
  // }
  // var out = _[i];
  // _ = __;
  // return [out];
};
exports.lng = () => _.length;
exports.setup = (i) => () => {
  _ = [];
};
exports.teardown = (i) => () => {
  _ = [];
};
exports.init = (l) => () => {
  _ = l;
};
