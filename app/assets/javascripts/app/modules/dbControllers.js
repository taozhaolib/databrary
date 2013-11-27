define([
	'angular',
	'ngAnimate',
	'ngStorage'
], function (angular) {
	'use strict';

	var dbControllers = angular.module('dbControllers', ['ngStorage', 'ngAnimate']);

	return dbControllers;
});
