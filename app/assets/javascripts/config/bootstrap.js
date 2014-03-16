define([
	'require',
	'jquery',
	'angular',
	'domReady',

	'./module',

	'config/controllers',
	'config/directives',
	'config/filters',
	'config/helpers',
	'config/interceptors',
	'config/models',
	'config/panels',
	'config/services',
	'config/views',

	'./routes'
], function (require, $, angular, domReady) {
	'use strict';

	domReady(function (document) {
		angular.bootstrap(document, ['databraryModule']);
	});
});
