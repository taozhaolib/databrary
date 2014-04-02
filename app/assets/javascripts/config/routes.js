define(['config/module'], function (module) {
	'use strict';

	module.config(['$locationProvider', '$routeProvider', function ($locationProvider, $routeProvider) {
		$locationProvider.html5Mode(true);

		//

		$routeProvider.when('/', {
			controller: 'WelcomeView',
			templateUrl: 'welcomeView.html',
			reloadOnSearch: false
		});

		//

		$routeProvider.when('/login', {
			controller: 'LoginView',
			templateUrl: 'loginView.html',
			reloadOnSearch: false
		});

		//

		$routeProvider.when('/register', {
			controller: 'RegisterView',
			templateUrl: 'registerView.html',
			reloadOnSearch: false
		});

		//

		$routeProvider.when('/password', {
			controller: 'ResetView',
			templateUrl: 'resetView.html',
			reloadOnSearch: false
		});

		//

		$routeProvider.when('/search', {
			controller: 'SearchView',
			templateUrl: 'searchView.html',
			resolve: {
				volumes: ['$route', 'Volume', '$q', function ($route, Volume, $q) {
					var deferred = $q.defer();

					Volume.query({}, function (data) {
						deferred.resolve(data);
					}, function (data) {
						deferred.reject();
					});

					return deferred.promise;
				}]
			},
			reloadOnSearch: false
		});

		//

		var partyView = {
			controller: 'PartyView',
			templateUrl: 'partyView.html',
			resolve: {
				party: ['$route', 'Party', '$q', '$window', 'TypeService', 'AuthService', function ($route, Party, $q, $window, type, auth) {
					var deferred = $q.defer();

					var req = {
						comments: '',
						access: '',
						parents: '',
						children: ''
					};

					if ($route.current.params.id)
						req.id = $route.current.params.id;
					else if (auth.isLoggedIn())
						req.id = auth.user.id;
					else if (type.isParty($window.$play.object))
						req.id = $window.$play.object.id;

					Party.get(req, function (data) {
						deferred.resolve(data);
					}, function (error) {
						deferred.reject();
					});

					return deferred.promise;
				}],
				volumes: ['$route', 'Volume', '$q', 'AuthService', 'TypeService', '$window', function ($route, Volume, $q, auth, type, $window) {
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
				}]
			},
			reloadOnSearch: false
		};

		$routeProvider.when('/party/:id', partyView);
		$routeProvider.when('/profile', partyView);

		//

		$routeProvider.when('/volume/:id', {
			controller: 'VolumeView',
			templateUrl: 'volumeView.html',
			resolve: {
				volume: ['$route', 'Volume', '$q', function ($route, Volume, $q) {
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
					}, function (data) {
						deferred.resolve(data);
					}, function (error) {
						deferred.reject();
					});

					return deferred.promise;
				}]
			},
			reloadOnSearch: false
		});

		//

		$routeProvider.when('/loading', {
			controller: 'LoadingView',
			templateUrl: 'loadingView.html',
			resolve: {
				authService: ['AuthService', function (auth) {
					return auth.$promise;
				}],
				constantService: ['ConstantService', function (constants) {
					return constants.$promise;
				}]
			},
			reloadOnSearch: false
		});

		//

		$routeProvider.otherwise({
			redirectTo: '/search'
		});
	}]);

	module.run(['$rootScope', 'RouterService', 'ConstantService', 'AuthService', '$location', function ($rootScope, router, constants, auth, $location) {
		var loaded = false;

		$rootScope.$on('$routeChangeStart', function (event, next, current) {
			if (!loaded) {
				if (!current)
					auth.next = $location.url();
				else if (current.$$route.controller == 'LoadingView')
					loaded = true;

				$location.url('/loading');
				return;
			}

			if (auth.isLoggedIn()) {
				if (auth.isUnauthorized()) {
					if (!next.$$route || next.$$route.controller != 'RegisterView')
						$location.url(router.register());
					return;
				} else if (next.$$route && ['ResetView', 'WelcomeView', 'LoginView', 'RegisterView'].indexOf(next.$$route.controller) != -1) {
					$location.url(router.search());
				} else if (auth.next) {
					$location.url(auth.next).replace();
					auth.next = undefined;
				}
			} else {
				if (auth.isPasswordPending() && next.$$route && next.$$route.controller != 'RegisterView') {
					$location.url(router.register());
				} else if (next.$$route && ['ResetView', 'LoadingView', 'WelcomeView', 'LoginView', 'RegisterView'].indexOf(next.$$route.controller) == -1) {
					auth.next = $location.url();
					$location.url(router.index());
				}
			}
		});
	}]);

	return module;
});
