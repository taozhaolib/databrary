define(['config/module'], function (module) {
	'use strict';

	module.directive('browserGroup', ['BrowserService', function (browserService) {
		var link = function ($scope) {
			$scope.browser = $scope.browser || browserService;

			//

			$scope.groupClasses = function (group) {
				var classes = [];

				if ($scope.browser.options[group].active)
					classes.push('on');
				else
					classes.push('off');

				if($scope.browser.isLastGroup(group))
					classes.push('last');

				return classes;
			};

			$scope.recordGroupClasses = function (group, $last) {
				var classes = [];

				classes.push('on');
				classes.push($scope.browser.isRecordGroupToggle(group) ? 'expanded' : '');

				if($last && !$scope.browser.canAddRecordGroup())
					classes.push('last');

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
