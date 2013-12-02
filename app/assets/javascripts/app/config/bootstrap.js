define([
	'require',
	'angular',
	'domReady',
	'app/modules/dbModule',
	'./routes'
], function (require, angular, domReady) {
	'use strict';

	domReady(function (document) {
		angular.bootstrap(document, ['dbModule']);
	});
});
