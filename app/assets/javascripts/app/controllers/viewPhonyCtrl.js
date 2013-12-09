define([
	'jquery',
	'app/modules/dbControllers'
], function ($, db) {
	'use strict';

	db.controller('ViewPhonyCtrl', ['$scope', '$routeParams', '$http', '$templateCache', function ($scope, $routeParams, $http, $templateCache) {
		$scope.phonyTemplateUrl = undefined;

		//

		var initialize = function () {
			$http
				.get('/'+$routeParams.page)
				.success(function (data, status, headers, config) {
					$templateCache.put($routeParams.page, $(data).filter('#main').text());
					$scope.phonyTemplateUrl = $routeParams.page;
				})
				.error(function (data, status, headers, config) {

				});
		};

		initialize();
	}]);
});
