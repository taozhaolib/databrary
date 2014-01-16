define(['app/config/module'], function (module) {
	'use strict';

	module.directive('browserSort', ['BrowserService', '$filter', function (browserService, $filter) {
		var compile = function ($element, $attrs, transclude) {
			return function ($scope, $element, $attrs) {
				$scope.toggleSort = function (sort) {
					$scope.browser.sort[sort].active = !$scope.browser.sort[sort].active;
				};

				$scope.sortClasses = function (sort) {
					var classes = [];

					if($scope.browser.sort.hasOwnProperty(sort) && $scope.browser.sort[sort].active)
						classes.push('on');
					else
						classes.push('off');

					return classes;
				};

				//

				$scope.switchRecordSort = function (sort, maybe) {
					recordToggleState = undefined;

					var sort_i = $scope.browser.recordSorts.index(sort),
						maybe_i = $scope.browser.recordSorts.index(maybe);

					if(sort.active != maybe.active) {
						sort.active = !sort.active;
						maybe.active = !maybe.active;
					}

					$scope.browser.recordSorts[sort_i] = $scope.browser.recordSorts.splice(maybe_i, 1, $scope.browser.recordSorts[sort_i])[0];
				};

				$scope.addRecordSort = function () {
					var go = true;

					angular.forEach($scope.browser.recordSorts, function (recordSort) {
						if(go && !recordSort.active) {
							recordSort.active = true;
							go = false;
						}
					});
				};

				$scope.canAddRecordSort = function () {
					var canAdd = false;

					angular.forEach($scope.browser.recordSorts, function (recordSort) {
						if(!canAdd && !recordSort.active) {
							canAdd = true;
						}
					});

					return canAdd;
				};

				$scope.removeRecordSort = function (sort) {
					sort.active = false;
				};

				$scope.canRemoveRecordSort = function () {
					return true;
				};

				$scope.recordSortClasses = function (sort) {
					var classes = [];

					classes.push('on');

					classes.push(recordToggleState == sort ? 'expanded' : '');

					return classes;
				};

				var recordToggleState = undefined;

				$scope.toggleRecordOptions = function (sort) {
					recordToggleState = angular.isUndefined(recordToggleState) ? sort : undefined;
				};
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
