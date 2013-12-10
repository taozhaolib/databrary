define([
	'jquery',
	'app/modules/dbControllers'
], function ($, db) {
	'use strict';

	db.controller('ViewPhonyCtrl', ['$scope', '$routeParams', '$http', '$templateCache', function ($scope, $routeParams, $http, $templateCache) {
		var start = function () {
			$scope.phonyTemplateUrl = undefined;

			console.log('ViewPhonyCtrl still catches this page!');

			$http
				.get('/'+$routeParams.page)
				.success(function (data) {
					$templateCache.put($routeParams.page, $(data).filter('#main').text());
					$scope.phonyTemplateUrl = $routeParams.page;
				})
				.error(function (data, status, headers, config) {

				});
		};

		start();
	}]);
});
