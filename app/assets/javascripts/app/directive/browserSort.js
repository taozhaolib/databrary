define(['app/config/module'], function (module) {
	'use strict';

	module.directive('browserSort', ['BrowserService', '$filter', function (browserService, $filter) {
		var compile = function ($element, $attrs, transclude) {
			return function ($scope, $element, $attrs) {
				$scope.toggleSort = function (sort) {
					$scope.browser.options[sort].active = !$scope.browser.options[sort].active;

					$scope.browser.updateData();
				};

				$scope.sortClasses = function (sort) {
					var classes = [];

					if($scope.browser.options.hasOwnProperty(sort) && $scope.browser.options[sort].active)
						classes.push('on');
					else
						classes.push('off');

					return classes;
				};

				//

				$scope.switchRecordSort = function (sort, maybe) {
					recordToggleState = undefined;

					var sort_i = $scope.browser.options.record.categories.index(sort),
						maybe_i = $scope.browser.options.record.categories.index(maybe);

					if(sort.active != maybe.active) {
						sort.active = !sort.active;
						maybe.active = !maybe.active;
					}

					$scope.browser.options.record.categories[sort_i] = $scope.browser.options.record.categories.splice(maybe_i, 1, $scope.browser.options.record.categories[sort_i])[0];

					$scope.browser.updateData();
				};

				$scope.addRecordSort = function () {
					var go = true;

					angular.forEach($scope.browser.options.record.categories, function (recordSort) {
						if(go && !recordSort.active) {
							recordSort.active = true;
							go = false;
						}
					});

					$scope.browser.updateData();
				};

				$scope.canAddRecordSort = function () {
					var canAdd = false;

					angular.forEach($scope.browser.options.record.categories, function (recordSort) {
						if(!canAdd && !recordSort.active) {
							canAdd = true;
						}
					});

					return canAdd;
				};

				$scope.removeRecordSort = function (sort) {
					sort.active = false;

					// move to end
					var sort_i = $scope.browser.options.record.categories.index(sort);

					$scope.browser.options.record.categories.splice(sort_i, 1);
					$scope.browser.options.record.categories.push(sort);

					$scope.browser.updateData();
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
