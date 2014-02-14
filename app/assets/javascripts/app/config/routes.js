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
						parents: '',
						children: '',
						funding: ''
					}, function (data) {
						deferred.resolve(data);
					}, function (error) {
						deferred.reject();
					});

					return deferred.promise;
				}],
				volumes: ['$route', 'Volume', '$q', function ($route, Volume, $q) {
					var deferred = $q.defer();

					Volume.query({
						party: $route.current.params.id
					}, function (data) {
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

		$routeProvider.when('/record/:id', {
			resolve: {
				redirect: ['$location', 'Record', function ($location, Record) {
					Record.get({}, function (data) {
						$location.path('/volume/'+data.volume);
					});
				}]
			}
		});

		$routeProvider.when('/slot/:id', {
			resolve: {
				redirect: ['$location', 'Slot', function ($location, Slot) {
					Slot.get({}, function (data) {
						$location.path('/volume/'+data.volume);
					});
				}]
			}
		});

		$routeProvider.when('/slot/:sid/asset/:id', {
			resolve: {
				redirect: ['$location', 'SlotAsset', function ($location, SlotAsset) {
					SlotAsset.get({}, function (data) {
						$location.path('/volume/'+data.volume);
					});
				}]
			}
		});

		$routeProvider.when('/asset/:id', {
			resolve: {
				redirect: ['$location', 'Asset', function ($location, Asset) {
					Asset.get({}, function (data) {
						$location.path('/volume/'+data.volume);
					});
				}]
			}
		});

		// TODO: implement real editing and remove phonyView

		$routeProvider.when('/:page*', {
			controller: 'PhonyView',
			templateUrl: 'phonyView.html',
			reloadOnSearch: false
		});

		//

		$routeProvider.otherwise({
			redirectTo: '/search'
		});
	}]);

	return module;
});
