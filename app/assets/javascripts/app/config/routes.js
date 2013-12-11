define(['app/config/module'], function (module) {
	'use strict';

	return module.config(['$locationProvider', '$routeProvider', function ($locationProvider, $routeProvider) {
		$locationProvider.html5Mode(true);

		//

		var staticView = {
			controller: 'StaticView',
			templateUrl: 'staticView.html',
			reloadOnSearch: false
		};

		$routeProvider.when('/', staticView);
		$routeProvider.when('/about/:page*', staticView);

		//

		var SearchView = {
			controller: 'SearchView',
			templateUrl: 'SearchView.html',
			reloadOnSearch: false
		};

		$routeProvider.when('/search', SearchView);

		//

		// TODO: route redirects need to be replaced with panel modes

		var partyView = {
			controller: 'PartyView',
			templateUrl: 'partyView.html',
			reloadOnSearch: false
		};

		$routeProvider.when('/party/:pid', partyView);
		$routeProvider.when('/party/:pid/:path*', {
			redirectTo: function (params) {
				return '/party/' + params.pid;
			}
		});

		//

		var assetView = {
			controller: 'AssetView',
			templateUrl: 'assetView.html',
			reloadOnSearch: false
		};

		$routeProvider.when('/volume/:vid/slot/:sid/asset/:aid', assetView);
		$routeProvider.when('/volume/:vid/slot/:sid/asset/:aid/:path*', {
			redirectTo: function (params) {
				return '/volume/' + params.vid + '/slot/' + params.sid + '/asset/' + params.aid;
			}
		});

		$routeProvider.when('/volume/:vid/asset/:aid', assetView);
		$routeProvider.when('/volume/:vid/asset/:aid/:path*', {
			redirectTo: function (params) {
				return '/volume/' + params.vid + '/asset/' + params.aid;
			}
		});

		//

		var slotView = {
			controller: 'SlotView',
			templateUrl: 'slotView.html',
			reloadOnSearch: false
		};

		$routeProvider.when('/volume/:vid/slot/:sid', slotView);
		$routeProvider.when('/volume/:vid/slot/:sid/:path*', {
			redirectTo: function (params) {
				return '/volume/' + params.vid + '/slot/' + params.sid;
			}
		});

		//

		var recordView = {
			controller: 'RecordView',
			templateUrl: 'recordView.html',
			reloadOnSearch: false
		};

		$routeProvider.when('/volume/:vid/record/:rid', recordView);
		$routeProvider.when('/volume/:vid/record/:rid/:path*', {
			redirectTo: function (params) {
				return '/volume/' + params.vid + '/record/' + params.rid;
			}
		});

		//

		var volumeView = {
			controller: 'VolumeView',
			templateUrl: 'volumeView.html',
			reloadOnSearch: false
		};

		$routeProvider.when('/volume/:vid', volumeView);
		$routeProvider.when('/volume/:vid/:path*', {
			redirectTo: function (params) {
				return '/volume/' + params.vid;
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
