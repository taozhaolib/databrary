define(['app/config/module'], function (module) {
	'use strict';

	module.directive('browserSort', ['BrowserService', function (browserService) {
		var compile = function ($element, $attrs, transclude) {
			return function ($scope, $element, $attrs) {
				$scope.toggleSort = function (sort) {
					$scope.browser.sort[sort] = !$scope.browser.sort[sort];
					console.log(sort, $scope.browser.sort[sort]);
				};

				$scope.sortClasses = function (sort) {
					var classes = [];

					if($scope.browser.sort[sort])
						classes.push('on');
					else
						classes.push('off');

					return classes;
				};

				//

				$scope.switchRecordSort = function () {

				};

				$scope.canAddRecordSort = function () {
					return $scope.browser.sort.record.length < $scope.browser.recordSorts.length;
				};

				$scope.canRemoveRecordSort = function () {
					return true;
				};

				$scope.recordSortClasses = function (sort) {
					var classes = [];

					classes.push('on');

					return classes;
				}
			};
		};

		return {
			restrict: 'E',
			scope: true,
			templateUrl: 'browserSort.html',
			replace: true,
			priority: 100,
			compile: compile
		};
	}]);
});
