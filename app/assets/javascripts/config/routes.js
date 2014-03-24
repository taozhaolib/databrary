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

		$routeProvider.when('/party/:id', {
			controller: 'PartyView',
			templateUrl: 'partyView.html',
			resolve: {
				party: ['$route', 'Party', '$q', function ($route, Party, $q) {
					var deferred = $q.defer();

					Party.get({
						volumes: '',
						comments: '',
						access: '',
						parents: '',
						children: '',
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

		$routeProvider.when('/volume/:id', {
			controller: 'VolumeView',
			templateUrl: 'volumeView.html',
			resolve: {
				volume: ['$route', 'Volume', '$q', function ($route, Volume, $q) {
					var deferred = $q.defer();

					Volume.get({
						access: '',
						citations: '',
						top: '',
						tags: '',
						assets: '',
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

		$routeProvider.otherwise({
			redirectTo: '/search'
		});
	}]);

	module.run(['$rootScope', 'RouterService', 'ConstantService', 'AuthService', '$location', function ($rootScope, router, constants, auth, $location) {
		$rootScope.$on('$routeChangeStart', function (event, next, current) {
//			if(auth.isUnauthorized() && next.$$route.controller != 'RegisterView') {
//					$location.url(router.register());
//			} else if(!auth.isLoggedIn()) {
//				if(auth.isPasswordPending() && next.$$route.controller != 'RegisterView') {
//					$location.url(router.register());
//				} else if (['WelcomeView', 'LoginView', 'RegisterView'].indexOf(next.$$route.controller) == -1) {
//					$location.url(router.index());
//				}
//			} else if (next.$$route.controller == 'WelcomeView') {
//				$location.url(router.search());
//			}

			if(angular.isUndefined(current) || $location.url() != next.$$route) {
				if(!next.resolve)
					next.resolve = {};

				next.resolve['promiseConstants'] = function () {
					return constants.$promise;
				};
			}
		});
	}]);

	return module;
});
