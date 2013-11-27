define([
	'angular',

	'./dbControllers',
	'app/config/controllers',

	'./dbDirectives',
	'app/config/directives',

	'./dbFilters',
	'app/config/filters',

	'./dbServices',
	'app/config/services',

	'ngRoute',
	'ngSanitize'
], function (angular) {
	'use strict';

	var dbModule = angular.module('dbModule', [
		'dbControllers',
		'dbDirectives',
		'dbFilters',
		'dbServices',

		'ngRoute',
		'ngSanitize'
	]);

	return dbModule;
});
