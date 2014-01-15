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
					return Volume.query({
						query: '',
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
					});
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
				party: ['$route', 'Party', function ($route, Party) {
					return Party.get({
						volumes: '',
						comments: '',
						parents: '',
						children: '',
						funding: ''
					});
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
				asset: ['$route', 'Asset', function ($route, Asset) {
					return Asset.get({
						slot: '',
						revisions: ''
					});
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
				slotAsset: ['$route', 'SlotAsset', function ($route, SlotAsset) {
					return SlotAsset.get({
						slot: '',
						revisions: ''
					});
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
				slot: ['$route', 'Slot', function ($route, Slot) {
					return Slot.get({
						tags: '',
						assets: '',
						comments: '',
						records: ''
					});
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
				record: ['$route', 'Record', function ($route, Record) {
					return Record.get({
						slots: ''
					});
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
				volume: ['$route', 'Volume', function ($route, Volume) {
					return Volume.get({
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
					});
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

	module.run(['$rootScope', 'RouterService', function ($rootScope, router) {
		$rootScope.router = router;
	}]);

	return module;
});
