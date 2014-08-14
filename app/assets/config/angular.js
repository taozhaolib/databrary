'use strict';

module.config([
  function () {
    angular.isNothing = function (val) {
      return val === undefined || val === null;
    };
  }
]);
