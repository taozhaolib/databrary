'use strict';

module.factory('volume', [
  '$resource', '$route', function ($resource, $route) {
    return $resource('/api/volume/:id', {
      id: function () {
        return $route.current.params.id || undefined;
      }
    }, null, {
      cache: 'volume'
    });
  }
]);
