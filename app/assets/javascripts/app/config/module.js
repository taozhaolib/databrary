define([
	'angular',

	'ngAnimate',
	'ngCookies',
	'ngRoute',
	'ngSanitize',
	'ngStorage'
], function (angular) {
	'use strict';

	var module = angular.module('databraryModule', [
		'ngAnimate',
		'ngCookies',
		'ngRoute',
		'ngSanitize',
		'ngStorage'
	]);

	module.config(['$logProvider', function ($logProvider) {
		$logProvider.debugEnabled(true);
	}]);

	module.run(function ($rootScope, $location) {
		// TODO: anchor scroll on page load. I think the problem is animated items on load.
		$location.hash('');
	});

	return module;
});
