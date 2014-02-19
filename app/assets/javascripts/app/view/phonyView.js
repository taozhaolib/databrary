define(['app/config/module'], function (module) {
	'use strict';

	module.controller('PhonyView', ['$scope', '$location', '$http', '$templateCache', function ($scope, $location, $http, $templateCache) {
		var start = function () {
			$scope.phonyTemplateUrl = undefined;

			console.log('PhonyView still catches this page!', $location.url());

			$http
				.get($location.url())
				.success(function (data) {
					$templateCache.put($location.url(), $(data).filter('#main').text());
					$scope.phonyTemplateUrl = $location.url();
				})
				.error(function () {});
		};

		start();
	}]);
});
