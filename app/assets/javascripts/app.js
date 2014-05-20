'use strict';

var module = angular.module('databraryModule', [
	'ngRoute',
	'ngSanitize',
	'ngStorage',
	'ngResource',
	'pasvaz.bindonce'
]);

module.config([
	'$httpProvider', '$logProvider', function ($httpProvider, $logProvider) {
		$httpProvider.defaults.headers.common['X-Requested-With'] = 'DatabraryClient';
		$httpProvider.defaults.cache = true;

		$logProvider.debugEnabled(true);
	}
]);

module.run([
	'$rootScope',
	'browserService',
	'authService',
	'typeService',
	'playService',
	'pageService',
	function ($rootScope, browser, auth, type, playService, page) {
		$rootScope.page = page;

		// TODO: move these
		$rootScope.browser = browser;
		$rootScope.auth = auth;
		$rootScope.type = type;

		// TODO: someday...
		//		playService.run();
	}
]);

