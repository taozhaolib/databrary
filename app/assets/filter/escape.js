'use strict';

module.filter('escape', [
  '$sce',
  function ($sce) {
    var ent = Object.freeze({
      "&": "&amp;",
      "<": "&lt;",
      ">": "&gt;",
      '"': "&quot;",
      "'": "&apos;"
    });

    return function (input) {
      return $sce.trustAsHtml(input.replace(/[&<>"']/g, function (c) {
        return ent[c];
      }));
    };
  }
]);
