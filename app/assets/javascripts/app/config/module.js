define([
	'angular',

	'ngAnimate',
	'ngCookies',
	'ngRoute',
	'ngSanitize',
	'ngStorage',

	'ngResource',
	'bindonce'
], function (angular) {
	'use strict';

	var module = angular.module('databraryModule', [
		'ngAnimate',
		'ngCookies',
		'ngRoute',
		'ngSanitize',
		'ngStorage',
		'ngResource',
		'pasvaz.bindonce'
	]);

	module.config(['$httpProvider', '$logProvider', function ($httpProvider, $logProvider) {
		$httpProvider.defaults.cache = true;
		$httpProvider.defaults.headers.common['X-Requested-With'] = 'DatabraryClient';

		$logProvider.debugEnabled(true);
	}]);

	module.run(['$window', '$rootScope', '$location', 'BrowserService', function ($window, $rootScope, $location, browser) {
		if(angular.isDefined($window.$play) && $window.$play.redirect){
			switch(browser.getItemType($window.$play.object)) {
				case 'session':
					$location.path('/volume/'+$window.$play.object.volume);
					break;

				case 'record':
					$location.path();
					break;

				default:
					// asset
					$location.path();
					break;
			}
		}

		// TODO: anchor scroll on page load. I think the problem is animated items on load.
		$location.hash('');
	}]);

	module.run(['$rootScope', '$log', 'RouterService', 'BrowserService', 'ConstantService', function ($rootScope, $log, router, browser, constant) {
		$rootScope.$log = $log;

		$rootScope.router = router;
		$rootScope.browser = browser;
		$rootScope.constant = constant;
	}]);

	return module;
});
