define(['app/config/module'], function (module) {
	'use strict';

	return module.config(['$locationProvider', '$routeProvider', '$httpProvider', function ($locationProvider, $routeProvider, $httpProvider) {
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
		$routeProvider.when('/party/:id/:path*', {
			redirectTo: function (params) {
				return '/party/' + params.id;
			}
		});

		//

		var assetView = {
			controller: 'AssetView',
			templateUrl: 'assetView.html',
			reloadOnSearch: false
		};

		$routeProvider.when('/volume/:vid/slot/:sid/asset/:id', assetView);
		$routeProvider.when('/volume/:vid/slot/:sid/asset/:id/:path*', {
			redirectTo: function (params) {
				return '/volume/' + params.vid + '/slot/' + params.sid + '/asset/' + params.id;
			}
		});

		$routeProvider.when('/volume/:vid/asset/:id', assetView);
		$routeProvider.when('/volume/:vid/asset/:id/:path*', {
			redirectTo: function (params) {
				return '/volume/' + params.vid + '/asset/' + params.id;
			}
		});

		//

		var slotView = {
			controller: 'SlotView',
			templateUrl: 'slotView.html',
			reloadOnSearch: false
		};

		$routeProvider.when('/volume/:vid/slot/:id', slotView);
		$routeProvider.when('/volume/:vid/slot/:id/:path*', {
			redirectTo: function (params) {
				return '/volume/' + params.vid + '/slot/' + params.id;
			}
		});

		//

		var recordView = {
			controller: 'RecordView',
			templateUrl: 'recordView.html',
			reloadOnSearch: false
		};

		$routeProvider.when('/volume/:vid/record/:id', recordView);
		$routeProvider.when('/volume/:vid/record/:id/:path*', {
			redirectTo: function (params) {
				return '/volume/' + params.vid + '/record/' + params.id;
			}
		});

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
		$routeProvider.when('/volume/:id/:path*', {
			redirectTo: function (params) {
				return '/volume/' + params.id;
			}
		});

		//

		// TODO: replace PhonyView with actual controllers

		$routeProvider.when('/:page*', {
			controller: 'PhonyView',
			templateUrl: 'phonyView.html',
			reloadOnSearch: false
		});

		//

		$routeProvider.otherwise({
			redirectTo: '/'
		});
	}]);
});
