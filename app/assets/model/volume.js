'use strict';

module.factory('volume', [
  '$resource', '$route', 'routerService', function ($resource, $route, router) {
    return $resource('/api/volume/:id', {
      id: function () {
        return $route.current.params.id || undefined;
      }
    }, {
      create: router.action(router.controllers.VolumeApi.create, ['owner']),
    }, {
      cache: 'volume'
    });
  }
]);
