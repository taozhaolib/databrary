define(['app/config/module'], function (module) {
	'use strict';

	module.directive('browserList', ['BrowserService', '$filter', function (browserService) {
		var compile = function ($element, $attrs, transclude) {
			return function ($scope, $element, $attrs, browserListParent) {
				$scope.levelItems = function () {

				};

				$scope.levelType = function () {
					
				};
			};
		};

		return {
			restrict: 'E',
			scope: true,
			templateUrl: 'browserList.html',
			replace: true,
			priority: 100,
			require: '?^browserList',
			compile: compile
		};
	}]);
});
