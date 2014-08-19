'use strict';

module.factory('party', [
  '$resource', '$route', 'routerService', function ($resource, $route, router) {
    var party = $resource('/api/party/:id', {
      id: function () {
        return ($route.current && $route.current.params.id) || undefined;
      }
    }, {
      query: {
        method: 'GET',
        isArray: false
      },
      password: {
        method: 'POST',
        url: '/api/party/:id/password'
      },
      profile: {
        method: 'GET',
        url: '/api/profile'
      },
      user: {
        method: 'GET',
        url: '/api/user'
      },
      login: {
        method: 'POST',
        url: '/api/user/login'
      },
      logout: {
        method: 'POST',
        url: '/api/user/logout'
      },
      superuserOn: {
        method: 'POST',
        url: '/api/user/superuser/on'
      },
      superuserOff: {
        method: 'POST',
        url: '/api/user/superuser/off'
      }
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
