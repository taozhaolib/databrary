'use strict';

module.factory('partyAuthorize', [
  '$resource', '$route', 'routerService', 'authService', function ($resource, $route, router, auth) {
    return $resource('/api/party/:id/authorize/:partyId', {
      id: function () {
        return $route.current.params.id ? $route.current.params.id : auth.user.id || undefined;
      }
    }, {
      query: router.action(router.controllers.PartyApi.authorizeGet, ['id']),
      search: router.action(router.controllers.PartyApi.authorizeSearch, ['id'], {isArray: true}),
      apply: router.action(router.controllers.PartyApi.authorizeApply, ['id', 'partyId']),
    }, {
      cache: 'partyAuthorize'
    });
  }
]);
