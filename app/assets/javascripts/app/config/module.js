define([
	'angular',

	'ngAnimate',
	'ngCookies',
	'ngRoute',
	'ngSanitize',
	'ngStorage',
	'ngResource'
], function (angular) {
	'use strict';

	var module = angular.module('databraryModule', [
		'ngAnimate',
		'ngCookies',
		'ngRoute',
		'ngSanitize',
		'ngStorage',
		'ngResource'
	]);

	module.config(['$httpProvider', function ($httpProvider) {
		$httpProvider.defaults.cache = true;
		$httpProvider.defaults.headers.common['X-Requested-With'] = 'DatabraryClient';
	}]);

	module.config(['$logProvider', function ($logProvider) {
		$logProvider.debugEnabled(true);
	}]);

	module.run(['$rootScope', '$location', function ($rootScope, $location) {
		// TODO: anchor scroll on page load. I think the problem is animated items on load.
		$location.hash('');
	}]);

	module.run(['$rootScope', '$log', function ($rootScope, $log) {
		$rootScope.$log = $log;
	}]);

	module.run(['$rootScope', 'RouterService', function ($rootScope, router) {
		$rootScope.router = router;
	}]);

	module.run(['$rootScope', 'BrowserService', function ($rootScope, browser) {
		$rootScope.browser = browser;
	}]);

	return module;
});
