'use strict';

module.factory('volumeAccess', [
  '$resource', '$route', 'routerService', function ($resource, $route, router) {
    return $resource('/api/volume/:id/access/:partyId', {
      id: function () {
        return $route.current.params.id || undefined;
      },
      partyId: function () {
        return $route.current.params.partyId || undefined;
      }
    }, {
      search: router.action(router.controllers.VolumeApi.accessSearch, ['id'], {isArray: true}),
      saveFunding: router.action(router.controllers.VolumeApi.fundingChange, ['id', 'funderId']),
      deleteFunding: router.action(router.controllers.VolumeApi.fundingDelete, ['id', 'funderId']),
      searchFunding: router.action(router.controllers.VolumeApi.funderSearch, [], {isArray: true}),
    }, 'volumeAccess');
  }
]);
