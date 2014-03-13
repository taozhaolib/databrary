define(['app/config/module'], function (module) {
	'use strict';

	module.config(['$locationProvider', '$routeProvider', '$httpProvider', function ($locationProvider, $routeProvider, $httpProvider) {
		$locationProvider.html5Mode(true);

		//

		var loginView = {
			controller: 'LoginView',
			templateUrl: 'loginView.html',
			reloadOnSearch: false
		};

		$routeProvider.when('/login', loginView);

		//

		var registerView = {
			controller: 'RegisterView',
			templateUrl: 'registerView.html',
			reloadOnSearch: false
		};

		$routeProvider.when('/register', registerView);

		//

		var searchView = {
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
		};

		$routeProvider.when('/search', searchView);

		//

		var partyView = {
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
		};

		$routeProvider.when('/party/:id', partyView);

		//

		var volumeView = {
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
		};

		$routeProvider.when('/volume/:id', volumeView);

		//

		$routeProvider.otherwise({
			redirectTo: '/search'
		});
	}]);

	return module;
});
