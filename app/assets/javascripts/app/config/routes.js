define(['app/modules/dbModule'], function (dbModule) {
	'use strict';

	return dbModule.config(['$locationProvider', '$routeProvider', function ($locationProvider, $routeProvider) {
		$locationProvider.html5Mode(true);

		//

		var viewStaticCtrl = {
			controller: 'ViewStaticCtrl',
			templateUrl: 'viewStaticCtrl.html',
			reloadOnSearch: false
		};

		$routeProvider.when('/', viewStaticCtrl);
		$routeProvider.when('/about/:page*', viewStaticCtrl);

		//

		// TODO: route redirects need to be replaced with panel modes

		var viewPartyCtrl = {
			controller: 'ViewPartyCtrl',
			templateUrl: 'ViewPartyCtrl.html',
			reloadOnSearch: false
		};

		$routeProvider.when('/party/:pid', viewPartyCtrl);
		$routeProvider.when('/party/:pid/:path*', {
			redirectTo: function (params) {
				return '/party/' + params.pid;
			}
		});

		//

		var viewAssetCtrl = {
			controller: 'ViewAssetCtrl',
			templateUrl: 'ViewAssetCtrl.html',
			reloadOnSearch: false
		};

		$routeProvider.when('/volume/:vid/slot/:sid/asset/:aid', viewAssetCtrl);
		$routeProvider.when('/volume/:vid/slot/:sid/asset/:aid/:path*', {
			redirectTo: function (params) {
				return '/volume/' + params.vid + '/slot/' + params.sid + '/asset/' + params.aid;
			}
		});

		$routeProvider.when('/volume/:vid/asset/:aid', viewAssetCtrl);
		$routeProvider.when('/volume/:vid/asset/:aid/:path*', {
			redirectTo: function (params) {
				return '/volume/' + params.vid + '/asset/' + params.aid;
			}
		});

		//

		var viewSlotCtrl = {
			controller: 'ViewSlotCtrl',
			templateUrl: 'ViewSlotCtrl.html',
			reloadOnSearch: false
		};

		$routeProvider.when('/volume/:vid/slot/:sid', viewSlotCtrl);
		$routeProvider.when('/volume/:vid/slot/:sid/:path*', {
			redirectTo: function (params) {
				return '/volume/' + params.vid + '/slot/' + params.sid;
			}
		});

		//

		var viewRecordCtrl = {
			controller: 'ViewRecordCtrl',
			templateUrl: 'ViewRecordCtrl.html',
			reloadOnSearch: false
		};

		$routeProvider.when('/volume/:vid/record/:rid', viewRecordCtrl);
		$routeProvider.when('/volume/:vid/record/:rid/:path*', {
			redirectTo: function (params) {
				return '/volume/' + params.vid + '/record/' + params.rid;
			}
		});

		//

		var viewVolumeCtrl = {
			controller: 'ViewVolumeCtrl',
			templateUrl: 'ViewVolumeCtrl.html',
			reloadOnSearch: false
		};

		$routeProvider.when('/volume/:vid', viewVolumeCtrl);
		$routeProvider.when('/volume/:vid/:path*', {
			redirectTo: function (params) {
				return '/volume/' + params.vid;
			}
		});

		//

		// TODO: replace viewPhonyCtrl with actual controllers

		$routeProvider.when('/:page*', {
			controller: 'ViewPhonyCtrl',
			templateUrl: 'viewPhonyCtrl.html',
			reloadOnSearch: false
		});

		//

		$routeProvider.otherwise({
			redirectTo: '/'
		});
	}]);
});
