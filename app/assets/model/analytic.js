'use strict';

module.factory('analytic', [
  '$resource', function ($resource) {
    return $resource('/api/null', null, null, {cache: false});
  }
]);
