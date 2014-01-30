define(['app/config/module'], function (module) {
	'use strict';

	module.directive('browserGroup', ['BrowserService', '$filter', function (browserService, $filter) {
		var compile = function ($element, $attrs, transclude) {
			return function ($scope, $element, $attrs) {
				$scope.toggleGroup = function (group) {
					$scope.browser.options[group].active = !$scope.browser.options[group].active;

					$scope.browser.updateData();
				};

				$scope.groupClasses = function (group) {
					var classes = [];

					if($scope.browser.options.hasOwnProperty(group) && $scope.browser.options[group].active)
						classes.push('on');
					else
						classes.push('off');

					return classes;
				};

				//

				$scope.switchRecordGroup = function (group, maybe) {
					recordToggleState = undefined;

					var group_i = $scope.browser.options.record.categories.index(group),
						maybe_i = $scope.browser.options.record.categories.index(maybe);

					if(group.active != maybe.active) {
						group.active = !group.active;
						maybe.active = !maybe.active;
					}

					$scope.browser.options.record.categories[group_i] = $scope.browser.options.record.categories.splice(maybe_i, 1, $scope.browser.options.record.categories[group_i])[0];

					$scope.browser.updateData();
				};

				$scope.addRecordGroup = function () {
					var go = true;

					angular.forEach($scope.browser.options.record.categories, function (recordGroup) {
						if(go && !recordGroup.active) {
							recordGroup.active = true;
							go = false;
						}
					});

					$scope.browser.updateData();
				};

				$scope.canAddRecordGroup = function () {
					var canAdd = false;

					angular.forEach($scope.browser.options.record.categories, function (recordGroup) {
						if(!canAdd && !recordGroup.active) {
							canAdd = true;
						}
					});

					return canAdd;
				};

				$scope.removeRecordGroup = function (group) {
					group.active = false;

					// move to end
					var group_i = $scope.browser.options.record.categories.index(group);

					$scope.browser.options.record.categories.splice(group_i, 1);
					$scope.browser.options.record.categories.push(group);

					$scope.browser.updateData();
				};

				$scope.canRemoveRecordGroup = function () {
					return true;
				};

				$scope.recordGroupClasses = function (group) {
					var classes = [];

					classes.push('on');

					classes.push(recordToggleState == group ? 'expanded' : '');

					return classes;
				};

				var recordToggleState = undefined;

				$scope.toggleRecordOptions = function (group) {
					recordToggleState = angular.isUndefined(recordToggleState) ? group : undefined;
				};
			};
		};

		return {
			restrict: 'E',
			scope: true,
			templateUrl: 'browserGroup.html',
			replace: true,
			priority: 100,
			compile: compile
		};
	}]);
});
