'use strict';

module.provider('routerService', [
  '$routeProvider', 'routeData',
  function ($routeProvider, controllers) {
    function encodeUri(val) {
      return encodeURIComponent(val).
        replace(/%40/g, '@').
        replace(/%3A/gi, ':').
        replace(/%24/g, '$').
        replace(/%2C/gi, ',').
        replace(/%20/g, '+');
    }

    /* Add the query params to the url. */
    function urlParams(url, params) {
      if (!params) {
        return url;
      }

      var q = url.contains('?');

      angular.forEach(params, function (value, key) {
        if (value === null || value === undefined) {
          return;
        }

        if (!Array.isArray(value)) {
          value = [value];
        }

        value.forEach(function (v) {
          if (angular.isObject(v)) {
            v = angular.toJson(v);
          }

	  url += (q ? '&' : '?') + encodeUri(key) + '=' + encodeUri(v);
	  q = true;
        });
      });

      return url;
    }

    /* Apply a jsroute (from play's routeData) given the names of its arguments and optional data (or placeholders). */
    function getRoute(route, argNames, data) {
      var args;
      if (!argNames)
	argNames = [];
      var ph = arguments.length < 3;
      if (ph)
	args = argNames.map(function (a) { return ':' + a; });
      else if (data === undefined || data === null)
	args = [];
      else if (Array.isArray(data))
	args = data;
      else if (typeof data === 'object') {
	args = argNames.map(function (k) {
	  return k in data ? data[k] : null;
	});
      } else
	args = [data];
      var r = route.apply(null, args);
      if (ph) {
	var q = r.url.indexOf('?');
	if (q !== -1)
	  r.url = r.url.substr(0, q);
	r.url = r.url.replace(/%3A/gi, ':');
      }
      return r;
    }

    /* Add a route to $routeProvider and return a function to get a url given parameters. */
    function makeRoute(route, argNames, handler) {
      if (handler)
	$routeProvider.when(getRoute(route, argNames).url, handler);

      return function (data, params) {
	return urlParams(getRoute(route, argNames, data).url, params);
      };
    }

    var routes = {};

    routes.index = makeRoute(controllers.Site.start, [], {
      controller: 'homeView',
      templateUrl: 'homeView.html',
      resolve: {
        parties: [
          'pageService', function (page) {
            return page.models.Party.query({
              access: page.permission.CONTRIBUTE,
              institution: false
            });
          }
        ],
        volume: [
          'pageService', function (page) {
            if (page.models.Login.checkAccess(page.permission.SHARED))
              return page.models.Volume.get(8, ['access']);
            else
	      return {};
          }
        ]
      },
      reloadOnSearch: false,
      authenticate: false
    });

    //

    routes.login = makeRoute(controllers.LoginHtml.view, [], {
      controller: 'loginView',
      templateUrl: 'loginView.html',
      reloadOnSearch: false
    });

    //

    routes.register = makeRoute(controllers.LoginHtml.registration, [], {
      controller: 'registerView',
      templateUrl: 'registerView.html',
      reloadOnSearch: false
    });

    //

    routes.password = makeRoute(controllers.TokenHtml.getPassword, [], {
      controller: 'resetView',
      templateUrl: 'resetView.html',
      reloadOnSearch: false
    });

    //

    routes.helpFormats = makeRoute(controllers.AssetHtml.formats, [], {
      controller: 'helpFormatsView',
      templateUrl: 'helpFormatsView.html',
      reloadOnSearch: false,
      authenticate: true,
    });

    //

    makeRoute(controllers.TokenHtml.token, ['id'], {
      controller: (function () {
        return window.$play.object && window.$play.object.reset ? 'resetView' : 'registerView';
      }()),
      templateUrl: function () {
        return window.$play.object && window.$play.object.reset ? 'resetView.html' : 'registerView.html';
      },
      resolve: {
        token: [
          'pageService', function (page) {
            var deferred = page.$q.defer();

            if (page.$window.$play.object && page.$window.$play.object.auth) {
              deferred.resolve(page.$window.$play.object);
            } else {
	      page.router.http(page.router.controllers.TokenApi.token,
		page.$route.current.params.id, page.$route.current.params.auth)
		.success(function (res) {
                  page.$window.$play.object = res;
                  deferred.resolve(res);
                })
                .error(function (res) {
                  deferred.reject(res);
                  page.$location.url('/');
                });
            }

            return deferred.promise;
          }
        ]
      },
      reloadOnSearch: false
    });

    //

    routes.search = makeRoute(controllers.VolumeHtml.search, [], {
      controller: 'searchView',
      templateUrl: 'searchView.html',
      resolve: {
        volumes: [
          'pageService', function (page) {
            return page.models.Volume.query(page.$route.current.params);
          }
        ]
      },
      reloadOnSearch: false,
      authenticate: true
    });

    //

    var partyView = {
      controller: 'partyView',
      templateUrl: 'partyView.html',
      resolve: {
        party: [
          'pageService', function (page) {
            var req = ['comments', 'access', 'openid', 'parents', 'children', 'volumes'];
            if ('id' in page.$route.current.params)
              return page.models.Party.get(page.$route.current.params.id, req);
            else
              return page.models.Party.profile(req);
          }
        ],
      },
      reloadOnSearch: false,
      authenticate: true
    };

    routes.profile = makeRoute(controllers.PartyHtml.profile, [], partyView);
    routes.party = makeRoute(controllers.PartyHtml.view, ['id'], partyView);

    //

    routes.partyEdit = makeRoute(controllers.PartyHtml.edit, ['id'], {
      controller: 'partyEditView',
      templateUrl: 'partyEditView.html',
      resolve: {
        party: [
          'pageService', function (page) {
            return page.models.Party.get(page.$route.current.params.id, {'parents':'all', 'children':'all'});
          }
        ],
      },
      reloadOnSearch: false,
      authenticate: true
    });

    //

    var volumeEdit = {
      controller: 'volumeEditView',
      templateUrl: 'volumeEditView.html',
      resolve: {
        volume: [
          'pageService', function (page) {
            if (!('id' in page.$route.current.params))
	      return undefined;

            return page.models.Volume.get(page.$route.current.params.id,
	      ['access', 'citation', 'top', 'funding'])
	      .then(function (volume) {
		return volume.top.getSlot(volume.top.segment, ['assets'])
		  .then(function (top) {
		    return volume;
		  });
	      });
          }
        ],
      },
      reloadOnSearch: false,
      authenticate: true
    };

    routes.volumeCreate = makeRoute(controllers.VolumeHtml.add, ['owner'], volumeEdit);
    routes.volumeEdit = makeRoute(controllers.VolumeHtml.edit, ['id'], volumeEdit);

    //

    routes.volume = makeRoute(controllers.VolumeHtml.view, ['id'], {
      controller: 'volumeView',
      templateUrl: 'volumeView.html',
      resolve: {
        volume: [
          'pageService', function (page) {
            return page.models.Volume.get(page.$route.current.params.id,
	      ['access', 'citation', 'funding', 'providers', 'consumers', 'top', 'tags', 'excerpts', 'comments', 'records', 'summary', 'containers', 'categories']);
          }
        ]
      },
      reloadOnSearch: false,
      authenticate: true
    });

    //

    routes.slot = makeRoute(controllers.SlotHtml.view, ['vid', 'id', 'segment'], {
      controller: 'slotView',
      templateUrl: 'slotView.html',
      resolve: {
        slot: [
          'pageService', function (page) {
            return page.models.Volume.get(page.$route.current.params.vid,
	      ['records', 'containers'])
	      .then(function (volume) {
		return volume.getSlot(page.$route.current.params.id, page.$route.current.params.segment,
		  ['assets', 'tags', 'comments', 'records']);
	      });
	  }
        ]
      },
      reloadOnSearch: false,
      authenticate: true
    });

    //

    routes.slotAsset = makeRoute(controllers.SlotAssetHtml.view, ['sid', 'segment', 'id']);
    routes.record = makeRoute(controllers.RecordHtml.view, ['id']);
    routes.volumeThumb = makeRoute(controllers.VolumeController.thumb, ['id', 'size']);
    routes.assetThumb = makeRoute(controllers.SlotAssetController.thumb, ['sid', 'segment', 'id', 'size']);
    routes.assetDownload = makeRoute(controllers.SlotAssetController.download, ['sid', 'segment', 'id', 'inline']);
    routes.partyAvatar = makeRoute(controllers.PartyHtml.avatar, ['id', 'size']);
    routes.slotEdit = makeRoute(controllers.SlotHtml.edit, ['id', 'segment']);
    routes.assetEdit = makeRoute(controllers.AssetHtml.edit, ['id']);
    routes.recordEdit = makeRoute(controllers.RecordHtml.edit, ['id']);

    //

    $routeProvider.otherwise({
      redirectTo: '/search'
    });

    this.$get = [
      '$location', '$http', 'constantService',
      function ($location, $http, constants) {
	var router = {
	  controllers: controllers,
	  prevUrl: '/',
	};
	angular.extend(router, routes);

	/* Simple wrapper to $http(route(...)).
	 * Non-object arguments (or an initial array) are passed to route.
	 * The next argument is passed as data (POST) or params.
	 * An additional argument is passed as extra config to $http.
	 */
	router.http = function (route, args/*...*/) {
	  var i;
	  if (Array.isArray(args))
	    i = 2;
	  else {
	    for (i = 1; i < arguments.length; i ++)
	      if (angular.isObject(arguments[i]))
		break;
	    args = Array.prototype.slice.call(arguments, 1, i);
	  }
	  var r = route.apply(null, args);
	  if (i < arguments.length) {
	    if (r.method === 'POST' || r.method === 'PUT')
	      r.data = arguments[i];
	    else
	      r.params = arguments[i];
	    if (++i < arguments.length)
	      angular.extend(r, arguments[i]);
	  }
	  if (r.data instanceof FormData) {
	    if (!('transformRequest' in r))
	      r.transformRequest = angular.identity;
	    if (!('headers' in r))
	      r.headers = {};
	    // Not sure why we do this but it seems to be necessary:
	    if (!('Content-Type' in r.headers))
	      r.headers['Content-Type'] = undefined;
	  }
	  return $http(r);
	};

	router.permalink = function (url) {
	  return constants.url + url;
	};

	router.volumeCreate = function (owner) {
	  router.prevUrl = $location.path();
	  return routes.volumeCreate([owner]);
	};

	return router;
      }
    ];
  }
]);
