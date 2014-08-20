'use strict';

module.provider('routerService', [
  '$routeProvider',
  'routeData',
  function RouterProvider($routeProvider, controllers) {
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
            var deferred = page.$q.defer();

            page.models.party.query({
              access: page.permission.CONTRIBUTE,
              institution: false
            }, function (res) {
              deferred.resolve(res);
            }, function (res) {
              deferred.reject(res);
            });

            return deferred.promise;
          }
        ],
        volume: [
          'pageService', function (page) {
            var deferred = page.$q.defer();

            if (page.auth.isAuthorized()) {
              page.models.volume.get({
                id: 8,
                access: ''
              }, function (res) {
                deferred.resolve(res);
              }, function (res) {
                deferred.reject(res);
              });
            } else {
              deferred.resolve({});
            }

            return deferred.promise;
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
            var deferred = page.$q.defer();

            page.models.volume.query({}, function (res) {
              deferred.resolve(res);
            }, function (res) {
              deferred.reject(res);
            });

            return deferred.promise;
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
            var deferred = page.$q.defer();

            var req = {
              comments: '',
              access: '',
              openid: '',
              parents: '',
              children: '',
            };

            if (page.$route.current.params.id) {
              req.id = page.$route.current.params.id;
            } else if (page.auth.isLoggedIn()) {
              req.id = page.auth.user.id;
            } else if (page.types.isParty(page.$window.$play.object)) {
              req.id = page.$window.$play.object.id;
            }

            if (page.$route.current.params.id) {
              page.models.party.get(req, function (res) {
                deferred.resolve(res);
              }, function (res) {
                deferred.reject(res);
              });
            } else {
              page.models.party.profile(req, function (res) {
                deferred.resolve(res);
              }, function (res) {
                deferred.reject(res);
              });
            }

            return deferred.promise;
          }
        ],
        volumes: [
          'pageService', function (page) {
            var deferred = page.$q.defer();

            var req = {
              id: null
            };

            if (page.$route.current.params.id) {
              req.party = page.$route.current.params.id;
            } else if (page.auth.isLoggedIn()) {
              req.party = page.auth.user.id;
            } else if (page.types.isParty(page.$window.$play.object)) {
              req.party = page.$window.$play.object.id;
            }

            if (page.constants.data.locked && !page.auth.isAuthorized()) {
              return [];
            }

            page.models.volume.query(req, function (res) {
              deferred.resolve(res);
            }, function (res) {
              deferred.reject(res);
            });

            return deferred.promise;
          }
        ]
      },
      reloadOnSearch: false,
      authenticate: true
    };

    routes.profile = makeRoute(controllers.PartyHtml.profile, [], partyView);
    routes.party = makeRoute(controllers.PartyHtml.view, ['id'], partyView);

    //

    var partyEditParty;
    routes.partyEdit = makeRoute(controllers.PartyHtml.edit, ['id'], {
      controller: 'partyEditView',
      templateUrl: 'partyEditView.html',
      resolve: {
        party: [
          'pageService', function (page) {
            var deferred = page.$q.defer();

            page.models.party.get({
              id: page.$route.current.params.id,
              parents: '',
              children: '',
            }, function (res) {
              deferred.resolve(res);
            }, function (res) {
              deferred.reject(res);
            });

            partyEditParty = deferred.promise;
            return deferred.promise;
          }
        ],
        partyAuth: [
          'pageService', function (page) {
            var deferred = page.$q.defer();

            var empty = {
              parents: [],
              children: [],
            };

            partyEditParty.then(function (party) {
              if (page.auth.hasAccess(page.permission.ADMIN, party)) {
                page.models.partyAuthorize.query(function (res) {
                  deferred.resolve(res);
                }, function (res) {
                  deferred.reject(res);
                });
              } else {
                deferred.resolve(empty);
              }
            }, function () {
              deferred.resolve(empty);
            });

            return deferred.promise;
          }
        ],
      },
      reloadOnSearch: false,
      authenticate: true
    });

    //

    var volumeEditVolume;
    var volumeEdit = {
      controller: 'volumeEditView',
      templateUrl: 'volumeEditView.html',
      resolve: {
        volume: [
          'pageService', function (page) {
            var deferred = page.$q.defer();

            if (!page.$route.current.params.id) {
              deferred.resolve();
            } else {
              page.models.volume.get({
                access: '',
                citation: '',
                top: '',
                funding: '',
              }, function (res) {
                deferred.resolve(res);
              }, function (res) {
                deferred.reject(res);
              });
            }

            volumeEditVolume = deferred.promise;
            return volumeEditVolume;
          }
        ],
        slot: [
          'pageService', function (page) {
            var deferred = page.$q.defer();

            if (!page.$route.current.params.id) {
              deferred.resolve();
            } else {
              volumeEditVolume.then(function (volume) {
                page.models.slot.get({
                  id: volume.top.id,
                  vid: volume.id,
                  assets: '',
                }, function (res) {
                  deferred.resolve(res);
                }, function (res) {
                  deferred.reject(res);
                });
              });
            }

            return deferred.promise;
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
            var deferred = page.$q.defer();

            page.models.volume.get({
              access: '',
              citation: '',
              funding: '',
              providers: '',
              consumers: '',
              top: '',
              tags: '',
              excerpts: '',
              comments: '',
              records: '',
              summary: '',
              sessions: '',
              categories: ''
            }, function (res) {
              deferred.resolve(res);
            }, function (res) {
              deferred.reject(res);
            });

            return deferred.promise;
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
            var deferred = page.$q.defer();

            page.models.slot.get({
              assets: '',
              records: '',
              tags: '',
              comments: '',
            }, function (res) {
              deferred.resolve(res);
            }, function (res) {
              deferred.reject(res);
            });

            return deferred.promise;
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
    routes.assetLink = makeRoute(controllers.SlotAssetController.download, ['sid', 'segment', 'id', 'inline']);
    routes.partyAvatar = makeRoute(controllers.PartyHtml.avatar, ['id', 'size']);
    routes.slotEdit = makeRoute(controllers.SlotHtml.edit, ['id', 'segment']);
    routes.assetEdit = makeRoute(controllers.AssetHtml.edit, ['id']);
    routes.recordEdit = makeRoute(controllers.RecordHtml.edit, ['id']);

    //

    $routeProvider.otherwise({
      redirectTo: '/search'
    });

    this.$get = [
      '$location',
      '$http',
      'typeService',
      function ($location, $http, type) {
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
	  return $http(r);
	};

	/* Construct a (resource) action from and route. */
	router.action = function (route, argNames, config) {
	  var r = getRoute(route, argNames);
	  if (config)
	    angular.extend(r, config);
	  return r;
	};

	router.party = function (data) {
	  if (!type.isParty(data)) {
	    throw new Error('routerService.party() requires Party as first argument');
	  }

	  return routes.party([data.id]);
	};

	router.partyEdit = function (data, page) {
	  if (!type.isParty(data)) {
	    throw new Error('routerService.partyEdit() requires Party as first argument');
	  }

	  var params = {};
	  if (page)
	    params.page = page;

	  return routes.partyEdit([data.id], params);
	};

	router.volumeCreate = function (owner) {
	  router.prevUrl = $location.path();
	  return routes.volumeCreate([owner]);
	};

	router.volumeEdit = function (data, page) {
	  if (!type.isVolume(data)) {
	    throw new Error('routerService.volumeEdit() requires Volume as first argument');
	  }

	  var params = {};
	  if (page)
	    params.page = page;

	  return routes.volumeEdit([data.id], params);
	};

	router.record = function (data) {
	  if (!type.isRecord(data)) {
	    throw new Error('routerService.record() requires Record as first argument');
	  }

	  return routes.record([data.id]);
	};

	router.assetThumb = function (data) {
	  if (!type.isAsset(data)) {
	    throw new Error('routerService.assetThumb() requires Asset as first argument');
	  }

	  return routes.assetThumb([data.container.id, type.segmentString(data), data.asset.id, data.size]);
	};

	router.assetLink = function (data, inline) {
	  inline = data.inline || inline;
	  if (!type.isAsset(data)) {
	    if (!data || !data.sid || !data.id)
	      throw new Error('routerService.assetLink() requires Asset or object.id/.sid/.segment as first argument');

	    return routes.assetLink([data.sid, type.segmentJoin(data.segment), data.id, inline]);
	  }

	  return routes.assetLink([data.container.id, type.segmentString(data), data.asset.id, inline]);
	};

	router.partyAvatar = function (data, size, nonce) {
	  if (!type.isParty(data)) {
	    throw new Error('routerService.partyAvatar() requires Party as first argument');
	  }

	  if (!angular.isNumber(size)) {
	    size = 56;
	  }

	  var params = {};
	  if (nonce) {
	    params.nonce = nonce;
	  }

	  return routes.partyAvatar([data.id, size], params);
	};

	router.slot = function (volume, data, segment) {
	  if (type.isVolume(volume))
	    volume = volume.id;

	  if (type.isSlot(data)) {
	    segment = data.segment;
	    data = data.id;
	  }

	  return routes.slot([volume, data, type.segmentJoin(segment)]);
	};

	router.slotEdit = function (data) {
	  if (!type.isSlot(data)) {
	    throw new Error('routerService.slotEdit() requires Slot as first argument');
	  }

	  return routes.slotEdit([data.id, type.segmentString(data)]);
	};

	router.assetEdit = function (data) {
	  if (!type.isAsset(data)) {
	    throw new Error('routerService.assetEdit() requires Asset as first argument');
	  }

	  return routes.assetEdit([data.asset.id]);
	};

	router.recordEdit = function (data) {
	  if (!type.isRecord(data)) {
	    throw new Error('routerService.recordEdit() requires Record as first argument');
	  }

	  return routes.recordEdit([data.id]);
	};

	return router;
      }
    ];
  }
]);
