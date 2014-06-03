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
	'playService',
	'pageService',
	function ($rootScope, playService, page) {
		$rootScope.page = page;

		// TODO: someday...
		//		playService.run();
	}
]);
