'use strict';

module.factory('volumeAccess', [
  '$resource', '$route', function ($resource, $route) {
    return $resource('/api/volume/:id/access/:partyId', {
      id: function () {
        return $route.current.params.id || undefined;
      },
      partyId: function () {
        return $route.current.params.partyId || undefined;
      }
    }, {
      search: {
        method: 'GET',
        url: '/api/volume/:id/access/:partyId/search',
      },

      saveFunding: {
        method: 'POST',
        url: '/api/volume/:id/funding/:funderId',
      },

      deleteFunding: {
        method: 'DELETE',
        url: '/api/volume/:id/funding/:funderId',
      },

      searchFunding: {
        method: 'GET',
        url: '/api/funder',
        isArray: true,
      },
    }, 'volumeAccess');
  }
]);
