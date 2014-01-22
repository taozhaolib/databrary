define(['app/config/module'], function (module) {
	'use strict';

	module.config(['$locationProvider', '$routeProvider', '$httpProvider', function ($locationProvider, $routeProvider, $httpProvider) {
		$locationProvider.html5Mode(true);

		//

		$httpProvider.responseInterceptors.push('authInterceptor');

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
				volumes: ['$route', 'Volume', function ($route, Volume) {
					var volumes = [];

					Volume.query({}, function (data) {
						angular.forEach(data, function (volume) {
							volumes.push(Volume.get({
								id: volume.id,

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
							}));
						});
					});

					return volumes;
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
					}, function (data) {
						deferred.reject();
					});

					return deferred.promise;
				}]
			},
			reloadOnSearch: false
		};

		$routeProvider.when('/party/:id', partyView);

		// I think this will probably disapparate...

		var assetView = {
			controller: 'AssetView',
			templateUrl: 'assetView.html',
			resolve: {
				asset: ['$route', 'Asset', '$q', function ($route, Asset, $q) {
					var deferred = $q.defer();

					Asset.get({
						slot: '',
						revisions: ''
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

		$routeProvider.when('/asset/:id', assetView);

		//

		var slotAssetView = {
			controller: 'slotAssetView',
			templateUrl: 'slotAssetView.html',
			resolve: {
				slotAsset: ['$route', 'SlotAsset', '$q', function ($route, SlotAsset, $q) {
					var deferred = $q.defer();

					SlotAsset.get({
						slot: '',
						revisions: ''
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

		$routeProvider.when('/slot/:sid/asset/:id', slotAssetView);

		//

		var slotView = {
			controller: 'SlotView',
			templateUrl: 'slotView.html',
			resolve: {
				slot: ['$route', 'Slot', '$q', function ($route, Slot, $q) {
					var deferred = $q.defer();

					Slot.get({
						tags: '',
						assets: '',
						comments: '',
						records: ''
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

		$routeProvider.when('/slot/:id', slotView);

		//

		var recordView = {
			controller: 'RecordView',
			templateUrl: 'recordView.html',
			resolve: {
				record: ['$route', 'Record', '$q', function ($route, Record, $q) {
					var deferred = $q.defer();

					Record.get({
						slots: ''
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

		$routeProvider.when('/record/:id', recordView);

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
					}, function (data) {
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
