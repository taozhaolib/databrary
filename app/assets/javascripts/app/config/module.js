define([
	'angular',

	'ngAnimate',
	'ngCookies',
	'ngRoute',
	'ngSanitize',
	'ngStorage',
	'ngRestangular'
], function (angular) {
	'use strict';

	var module = angular.module('databraryModule', [
		'ngAnimate',
		'ngCookies',
		'ngRoute',
		'ngSanitize',
		'ngStorage',
	'ngRestangular'
	]);

	module.config(['$logProvider', function ($logProvider) {
		$logProvider.debugEnabled(true);
	}]);

	module.config(['RestangularProvider', function (RestangularProvider) {
		RestangularProvider.seBaseUrl('/api');
		// TODO; RestangularProvider.setDefaultHttpFields({cache: myCache}) where myCache has the special features I need: refreshing on authChange, checking whether var is ID (for return or GET) or object (for storage), etc
	}]);

	module.run(['$rootScope', '$location', function ($rootScope, $location) {
		// TODO: anchor scroll on page load. I think the problem is animated items on load.
		$location.hash('');
	}]);

	return module;
});
