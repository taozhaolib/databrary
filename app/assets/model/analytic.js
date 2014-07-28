'use strict';

module.factory('analytic', [
  'resourceFactory', function (resource) {
    var analytic = resource('/api/null', {}, 'analytic');

    analytic.send = function () {
      analytic.$cache.removeAll();
      analytic.get();
    };

    return analytic;
  }
]);
