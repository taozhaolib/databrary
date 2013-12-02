define([
	'angular',
	'ngCookies'
], function (angular) {
	'use strict';

	var dbServices = angular.module('dbServices', ['ngCookies']);

	return dbServices;
});
