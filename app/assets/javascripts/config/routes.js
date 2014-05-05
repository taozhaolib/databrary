module.config([
	'$locationProvider', '$routeProvider', '$httpProvider', function ($locationProvider, $routeProvider, $httpProvider) {
		$locationProvider.html5Mode(true);

		//

		var appResolve = function (config) {
			if (!config.resolve)
				config.resolve = {};

			angular.extend(config.resolve, {
				authPromise: [
					'authService', function (auth) {
						return auth.$promise;
					}
				],
				constantPromise: [
					'constantService', function (constants) {
						return constants.$promise;
					}
				]
			});

			return config;
		};

		//

		$routeProvider.when('/', appResolve({
			controller: 'WelcomeView',
			templateUrl: 'welcomeView.html',
			reloadOnSearch: false
		}));

		//

		$routeProvider.when('/login', appResolve({
			controller: 'LoginView',
			templateUrl: 'loginView.html',
			reloadOnSearch: false
		}));

		//

		$routeProvider.when('/register', appResolve({
			controller: 'RegisterView',
			templateUrl: 'registerView.html',
			reloadOnSearch: false
		}));

		//

		$routeProvider.when('/password', appResolve({
			controller: 'ResetView',
			templateUrl: 'resetView.html',
			reloadOnSearch: false
		}));

		//

		$routeProvider.when('/token/:id', appResolve({
			controller: function () {
				return window.$play.object && window.$play.object.reset ? 'ResetView' : 'RegisterView';
			},
			templateUrl: function () {
				return window.$play.object && window.$play.object.reset ? 'resetView.html' : 'registerView.html';
			},
			resolve: {
				token: ['$q', '$http', '$route', '$window', '$location', function ($q, $http, $route, $window, $location) {
					var deferred = $q.defer();

					if ($window.$play.object && $window.$play.object.auth)
						deferred.resolve($window.$play.object);
					else
						$http
							.get('/api/token/' + $route.current.params.id + '?auth=' + $route.current.params.auth)
							.success(function (data) {
								$window.$play.object = data;
								deferred.resolve(data);
							})
							.error(function () {
								deferred.reject();
								$location.url('/');
							});

					return deferred.promise;
				}]
			},
			reloadOnSearch: false
		}));

		//

		$routeProvider.when('/search', appResolve({
			controller: 'SearchView',
			templateUrl: 'searchView.html',
			resolve: {
				volumes: [
					'$route', 'Volume', '$q', function ($route, Volume, $q) {
						var deferred = $q.defer();

						Volume.query({}, function (data) {
							deferred.resolve(data);
						}, function (data) {
							deferred.reject();
						});

						return deferred.promise;
					}
				]
			},
			reloadOnSearch: false,
			authenticate: true
		}));

		//

		var partyView = appResolve({
			controller: 'PartyView',
			templateUrl: 'partyView.html',
			resolve: {
				party: [
					'$route',
					'Party',
					'$q',
					'$window',
					'typeService',
					'authService',
					function ($route, Party, $q, $window, type, auth) {
						var deferred = $q.defer();

						var req = {
							comments: '',
							access: '',
							openid: '',
							duns: '',
							parents: '',
							children: ''
						};

						if ($route.current.params.id)
							req.id = $route.current.params.id;
						else if (auth.isLoggedIn())
							req.id = auth.user.id;
						else if (type.isParty($window.$play.object))
							req.id = $window.$play.object.id;

						if ($route.current.params.id)
							Party.get(req, function (data) {
								deferred.resolve(data);
							}, function (error) {
								deferred.reject();
							});
						else
							Party.profile(req, function (data) {
								deferred.resolve(data);
							}, function (error) {
								deferred.reject();
							});

						return deferred.promise;
					}
				],
				volumes: [
					'$route',
					'Volume',
					'$q',
					'authService',
					'typeService',
					'$window',
					function ($route, Volume, $q, auth, type, $window) {
						var deferred = $q.defer();

						var req = {
							id: null
						};

						if ($route.current.params.id)
							req.party = $route.current.params.id;
						else if (auth.isLoggedIn())
							req.party = auth.user.id;
						else if (type.isParty($window.$play.object))
							req.party = $window.$play.object.id;

						Volume.query(req, function (data) {
							deferred.resolve(data);
						}, function (data) {
							deferred.reject();
						});

						return deferred.promise;
					}
				]
			},
			reloadOnSearch: false,
			authenticate: true
		});

		$routeProvider.when('/party/:id', partyView);
		$routeProvider.when('/profile', partyView);

		//

		var volumeEdit = {
			controller: 'VolumeEditView',
			templateUrl: 'volumeEditView.html',
			resolve: {
				volume: [
					'$route', 'Volume', '$q', function ($route, Volume, $q) {
						var deferred = $q.defer();

						if (!$route.current.params.id) {
							deferred.resolve();
						} else {
							Volume.get({
								access: '',
								citations: '',
								top: '',
								excerpts: '',
								funding: ''
							}, function (res) {
								deferred.resolve(res);
							}, function (res) {
								deferred.reject();
							});
						}

						return deferred.promise;
					}
				]
			},
			reloadOnSearch: false,
			authenticate: true
		};

		$routeProvider.when('/volume/create', appResolve(volumeEdit));
		$routeProvider.when('/volume/:id/edit', appResolve(volumeEdit));

		//

		$routeProvider.when('/volume/:id', appResolve({
			controller: 'VolumeView',
			templateUrl: 'volumeView.html',
			resolve: {
				volume: [
					'$route', 'Volume', '$q', function ($route, Volume, $q) {
						var deferred = $q.defer();

						Volume.get({
							access: '',
							citations: '',
							providers: '',
							consumers: '',
							top: '',
							tags: '',
							excerpts: '',
							comments: '',
							records: '',
							summary: '',
							sessions: '',
							categories: '',
							funding: ''
						}, function (res) {
							deferred.resolve(res);
						}, function (res) {
							deferred.reject();
						});

						return deferred.promise;
					}
				]
			},
			reloadOnSearch: false,
			authenticate: true
		}));

		//

		$routeProvider.otherwise({
			redirectTo: '/search'
		});

		//

		// TODO: circular dependency from authService
//		$httpProvider.interceptors.push([
//			'$q', '$location', 'authService',
//			function ($q, $location, auth) {
//				return {
//					'responseError': function (response) {
//						if (response.status == 403) {
//							auth.tryLogin();
//						}
//
//						return $q.reject(response);
//					}
//				};
//			}
//		]);
	}
]);

module.run([
	'$rootScope',
	'routerService',
	'constantService',
	'authService',
	'$location',
	function ($rootScope, router, constants, auth, $location) {
		$rootScope.$on('$routeChangeStart', function (event, next) {
			auth.$promise.then(function () {
				if (auth.isLoggedIn()) {
					if (auth.isUnauthorized()) {
						if (!next.$$route || next.$$route.controller != 'RegisterView' || (angular.isFunction(next.$$route.controller) && next.$$route.controller() != 'RegisterView'))
							$location.url(router.register());
					} else if (!next.authenticate) {
						$location.url(router.search());
					}
				} else {
					if (auth.isPasswordPending() && next.$$route && next.$$route.controller != 'RegisterView' && (!angular.isFunction(next.$$route.controller) || next.$$route.controller() != 'RegisterView')) {
						$location.url(router.register());
					} else if (next.authenticate) {
						$location.url(router.index());
					}
				}
			});
		});
	}
]);


