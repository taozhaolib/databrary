define(['app/config/module'], function (module) {
	'use strict';

	module.config(['$locationProvider', '$routeProvider', '$httpProvider', function ($locationProvider, $routeProvider, $httpProvider) {
		$locationProvider.html5Mode(true);

		//

		$httpProvider.responseInterceptors.push('authInterceptor');

		//

		var staticView = {
			controller: 'StaticView',
			templateUrl: 'staticView.html',
			reloadOnSearch: false
		};

		$routeProvider.when('/', staticView);
		$routeProvider.when('/about/:page*', staticView);

		//

		var LoginView = {
			controller: 'LoginView',
			templateUrl: 'loginView.html',
			reloadOnSearch: false
		};

		$routeProvider.when('/login', LoginView);

		//

		var SearchView = {
			controller: 'SearchView',
			templateUrl: 'searchView.html',
//			resolve: {
//				volumes: ['$route', 'Volume', function ($route, Volume) {
//					return Volume.query({
//						access: 'all',
//						top: 'all',
//						summary: 'all'
//					});
//				}]
//			},
			reloadOnSearch: false
		};

		$routeProvider.when('/search', SearchView);

		//

		// TODO: route redirects need to be replaced with panel modes

		var partyView = {
			controller: 'PartyView',
			templateUrl: 'partyView.html',
			resolve: {
				party: ['$route', 'Party', function ($route, Party) {
					return Party.get($route.current.params.id, {
						id: $route.current.params.id,
						volumes: 'all',
						comments: 'all',
						parents: 'all',
						children: 'all',
						funding: 'all'
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
					return Volume.get($route.current.params.id, {
						id: $route.current.params.id,
						access: 'all',
						citations: 'all',
						top: 'all',
						tags: 'all',
						assets: 'all',
						comments: 'all',
						records: 'all',
						summary: 'all',
						sessions: 'all',
						categories: 'all',
						funding: 'all'
					});
				}]
			},
			reloadOnSearch: false
		};

		$routeProvider.when('/volume/:id', volumeView);

		//

		$routeProvider.otherwise({
			redirectTo: '/'
		});
	}]);

	module.run(['$rootScope', 'RouterService', function ($rootScope, router) {
		$rootScope.router = router;
	}]);

	return module;
});
