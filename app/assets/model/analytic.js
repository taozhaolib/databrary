'use strict';

module.factory('analytic', [
  '$resource', function ($resource) {
    return $resource('/api/null');
  }
]);
