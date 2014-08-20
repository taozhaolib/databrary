'use strict';

module.factory('party', [
  '$resource', '$route', 'routerService', function ($resource, $route, router) {
    var party = $resource('/api/party/:id', {
      id: function () {
        return ($route.current && $route.current.params.id) || undefined;
      }
    }, {
      profile: router.action(router.controllers.PartyApi.profile),
      user: router.action(router.controllers.LoginApi.get),
      login: router.action(router.controllers.LoginApi.post),
      logout: router.action(router.controllers.LoginApi.logout),
      superuserOn: router.action(router.controllers.LoginApi.superuserOn),
      superuserOff: router.action(router.controllers.LoginApi.superuserOff),
    }, {
      cache: 'party'
    });

    party.upload = function (party, fd) {
      return router.http(router.controllers.PartyApi.update, party.id, fd, {
        transformRequest: angular.identity,
        headers: {
          'Content-Type': undefined
        },
      });
    };

    return party;
  }
]);
