define([
	'require',
	'jquery',
	'angular',
	'domReady',

	'./module',

	'app/config/controllers',
	'app/config/directives',
	'app/config/filters',
	'app/config/helpers',
	'app/config/models',
	'app/config/panels',
	'app/config/services',
	'app/config/views',

	'./routes'
], function (require, $, angular, domReady) {
	'use strict';

	domReady(function (document) {
		angular.bootstrap(document, ['databraryModule']);
	});
});
