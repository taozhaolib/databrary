'use strict';

module.factory('record', [
  '$resource', '$route', 'routerService', function ($resource, $route, router) {
    return $resource('/api/record/:id', {
      id: function () {
        return $route.current.params.id || undefined;
      }
    }, {
      measureUpdate: router.action(router.controllers.RecordApi.measureUpdate, ['id', 'metric']),
    }, 'record');
  }
]);
