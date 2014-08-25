'use strict';

module.factory('slot', [
  '$resource', '$route', 'routerService', function ($resource, $route, router) {
    return $resource('/api/volume/:vid/slot/:id', {
      id: function () {
        return $route.current.params.id || undefined;
      },
      vid: function () {
        return $route.current.params.vid || undefined;
      },
      segment: function () {
        return $route.current.params.segment || ',';
      }
    }, {
      update: router.action(router.controllers.SlotApi.update, ['id', 'segment']),
      addRecord: router.action(router.controllers.RecordApi.add, ['id', 'segment']),
    }, {
      cache: 'slot'
    });
  }
]);
