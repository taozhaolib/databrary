'use strict';

module.factory('routerService', [
  '$rootScope',
  '$route',
  '$filter',
  '$location',
  'typeService',
  function ($rootScope, $route, $filter, $location, type) {
    var router = {};
    var prevUrl = '/';

    router.$route = $route;

    //

    var makeUrl = function (url, params) {
      if (!params) {
        return url;
      }

      if (!angular.isObject(params) || angular.isArray(params)) {
        throw new Error('routerService.makeUrl passed non-object "params" for ' + url);
      }

      var parts = [];

      angular.forEach(params, function (value, key) {
        if (value === null || value === undefined) {
          return;
        }

        if (!angular.isArray(value)) {
          value = [value];
        }

        value.forEach(function (v) {
          if (angular.isObject(v)) {
            v = angular.toJson(v);
          }

          var m = url.indexOf(':' + key);
          if (m >= 0) {
            var e = m + 1 + key.length;
            url = url.slice(0, m) + v + url.slice(e + (url[e] === '*'));
          } else {
            parts.push($filter('uri')(key, true) + '=' +
              $filter('uri')(v, true));
          }
        });
      });

      if (parts.length > 0) {
        url += ((url.indexOf('?') == -1) ? '?' : '&');
	url += parts.join('&');
      }

      return url;
    };

    //

    var makeRoute = function (route) {
      return function (params, setPrev) {
        if (setPrev) {
          prevUrl = $location.path();
        }
        return makeUrl(route, params);
      };
    };

    //

    router.index = makeRoute('/');
    router.login = makeRoute('/login');
    router.register = makeRoute('/register');
    router.password = makeRoute('/password');
    router.profile = makeRoute('/profile');
    router.error = makeRoute('/error');

    router.search = makeRoute('/search');
    router.asset = makeRoute('/asset/:id');
    router.volume = makeRoute('/volume/:id');
    router.volumeCreate = makeRoute('/volume/create');
    router.slot = makeRoute('/volume/:vid/slot/:id');
    router.slotAsset = makeRoute('/slot/:sid/asset/:id');
    router.helpFormats = makeRoute('/asset/formats');

    router.prevUrl = function () {
      return prevUrl;
    };
    //

    router.record = function (data) {
      if (!type.isRecord(data)) {
        throw new Error('routerService.record() requires Record as first argument');
      }

      data = {
        id: data.id
      };

      return makeUrl('/record/:id', data);
    };

    router.volumeThumb = function (data) {
      if (!type.isVolume(data)) {
        throw new Error('routerService.volumeThumb() requires Volume as first argument');
      }

      data = {
        id: data.id
      };

      return makeUrl('/volume/:id/thumb', data);
    };

    router.assetThumb = function (data) {
      if (!type.isAsset(data)) {
        throw new Error('routerService.assetThumb() requires Asset as first argument');
      }

      data = {
        sid: data.container.id,
        id: data.asset.id,
        segment: type.segmentString(data),
	size: data.size
      };

      return makeUrl('/slot/:sid/asset/:id/thumb', data);
    };

    router.assetHead = function (data) {
      if (!type.isAsset(data)) {
        throw new Error('routerService.assetHead() requires Asset as first argument');
      }

      data = {
        sid: data.container.id,
        id: data.asset.id,
        segment: type.segmentString(data)
      };

      return makeUrl('/slot/:sid/asset/:id/head', data);
    };

    router.assetLink = function (data, inline) {
      if (type.isAsset(data)) {
        data = {
          sid: data.container.id,
          id: data.asset.id,
          segment: type.segmentString(data)
        };
      } else if (!data || !data.sid || !data.id) {
        throw new Error('routerService.assetLink() requires Asset or object.id/.sid/.segment as first argument');
      }

      data.inline = data.inline || inline || false;

      return makeUrl('/slot/:sid/asset/:id/download', data);
    };

    router.partyAvatar = function (data, size, nonce) {
      if (!type.isParty(data)) {
        throw new Error('routerService.partyAvatar() requires Party as first argument');
      }

      if (!angular.isNumber(size)) {
        size = 56;
      }

      data = {
        id: data.id
      };

      if (angular.isNumber(parseInt(size))) {
        if (angular.isObject(data)) {
          data.size = parseInt(size);
        } else if (angular.isArray(data)) {
          data.push(parseInt(size));
        } else if (angular.isString(data)) {
          data = [data, size];
        } else {
          data = '';
        }
      }

      if (nonce) {
	data.nonce = nonce;
      }

      return makeUrl('/party/:id/avatar', data);
    };

    router.slotEdit = function (data) {
      if (!type.isSession(data)) {
        throw new Error('routerService.slotEdit() requires Slot as first argument');
      }

      data = {
        id: data.id,
        segment: type.segmentString(data)
      };

      return makeUrl('/slot/:id/edit', data);
    };

    router.assetEdit = function (data) {
      if (!type.isAsset(data)) {
        throw new Error('routerService.assetEdit() requires Asset as first argument');
      }

      data = {
        id: data.asset.id
      };

      return makeUrl('/asset/:id/edit', data);
    };

    router.recordEdit = function (data) {
      if (!type.isRecord(data)) {
        throw new Error('routerService.recordEdit() requires Record as first argument');
      }

      data = {
        id: data.id
      };

      return makeUrl('/record/:id/edit', data);
    };

    router.volumeEdit = function (data, page) {
      if (!type.isVolume(data)) {
        throw new Error('routerService.volumeEdit() requires Volume as first argument');
      }

      data = {
        id: data.id
      };

      if (page) {
        data.page = page;
      }

      return makeUrl('/volume/:id/edit', data);
    };

    router.partyEdit = function (data, page) {
      if (!type.isParty(data)) {
        throw new Error('routerService.partyEdit() requires Party as first argument');
      }

      data = {
        id: data.id
      };

      if (page) {
        data.page = page;
      }

      return makeUrl('/party/:id/edit', data);
    };

    router.party = function (data) {
      if (!type.isParty(data)) {
        throw new Error('routerService.party() requires Party as first argument');
      }

      data = {
        id: data.id
      };

      return makeUrl('/party/:id', data);
    };

    router.partyAuthorize = function (data) {
      if (!type.isParty(data)) {
        throw new Error('routerService.partyAuthorize() requires Party as first argument');
      }

      data = {
        id: data.id
      };

      return makeUrl('/party/:id/authorize', data);
    };

    //

    return router;
  }
]);
