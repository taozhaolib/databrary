define(['app/config/module'], function (module) {
	'use strict';

	module.directive('browserList', ['BrowserService', '$filter', function (browserService) {
		var compile = function ($element, $attrs, transclude) {
			return function ($scope, $element, $attrs, browserListParent) {
				$scope.levelItems = function () {

				};

				$scope.levelType = function () {

				};

				$scope.nextLevelType = function (type) {
					switch(type) {
						case 'asset':
							return undefined;

						case 'session':
							return 'asset';

						case 'volume':
							// blech. put this in Service.

						default:


							return undefined;
					}
				};

				$scope.levelInclude = function ()  {
					return $scope.levelType()+'.html';
				}
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
