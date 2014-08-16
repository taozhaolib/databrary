'use strict';

module.filter('escape', [
  function () {
    var ent = Object.freeze({
      "&": "&amp;",
      "<": "&lt;",
      ">": "&gt;",
      '"': "&quot;",
      "'": "&apos;"
    });

    return function (input) {
      return input.replace(/[&<>"']/g, function (c) {
        return ent[c];
      });
    };
  }
]);
