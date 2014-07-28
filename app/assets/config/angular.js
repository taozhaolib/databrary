'use strict';

module.config([
  function () {
    angular.isNothing = function (val) {
      return angular.isUndefined(val) || val === null;
    };
  }
]);
