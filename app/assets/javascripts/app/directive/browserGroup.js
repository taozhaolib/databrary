define(['app/config/module'], function (module) {
	'use strict';

	module.directive('browserGroup', ['BrowserService', '$filter', function (browserService, $filter) {
		var link = function ($scope, $element, $attrs) {
			$scope.browser = $scope.browser || browserService;

			//

			$scope.groupClasses = function (group) {
				var classes = [];

				if ($scope.browser.options[group].active)
					classes.push('on');
				else
					classes.push('off');

				return classes;
			};

			$scope.recordGroupClasses = function (group) {
				var classes = [];

				classes.push('on');
				classes.push($scope.browser.isRecordGroupToggle(group) ? 'expanded' : '');

				return classes;
			};
		};

		return {
			restrict: 'E',
			scope: true,
			templateUrl: 'browserGroup.html',
			replace: true,
			priority: 100,
			link: link
		};
	}]);
});
