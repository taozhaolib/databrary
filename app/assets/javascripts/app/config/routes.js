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
//			resolve: {
//				volumes: ['$route', 'Volume', function ($route, Volume) {
//					return Volume.query({
//						access: '',
//						top: '',
//						summary: ''
//					});
//				}]
//			},
			reloadOnSearch: false
		};

		$routeProvider.when('/search', searchView);

		//

		// TODO: route redirects need to be replaced with panel modes

		var partyView = {
			controller: 'PartyView',
			templateUrl: 'partyView.html',
			resolve: {
				party: ['$route', 'Party', function ($route, Party) {
					return Party.get({
						id: $route.current.params.id,
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

		//

		var assetView = {
			controller: 'AssetView',
			templateUrl: 'assetView.html',
			reloadOnSearch: false
		};

		$routeProvider.when('/slot/:sid/asset/:id', assetView);
		$routeProvider.when('/asset/:id', assetView);

		//

		var slotView = {
			controller: 'SlotView',
			templateUrl: 'slotView.html',
			reloadOnSearch: false
		};

		$routeProvider.when('/slot/:id', slotView);

		//

		var recordView = {
			controller: 'RecordView',
			templateUrl: 'recordView.html',
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
						id: $route.current.params.id,
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
