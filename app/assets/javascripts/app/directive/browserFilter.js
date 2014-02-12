define(['app/config/module'], function (module) {
	'use strict';

	module.directive('browserFilter', ['BrowserService', '$filter', function (browserService, $filter) {
		var link = function ($scope, $element, $attrs) {
			$scope.browser = $scope.browser || browserService;

			//

			$scope.sortClasses = function (sort) {
				var classes = [];

				if ($scope.browser.options[sort].active)
					classes.push('on');
				else
					classes.push('off');

				return classes;
			};
		};

		return {
			restrict: 'E',
			scope: true,
			templateUrl: 'browserFilter.html',
			replace: true,
			priority: 100,
			link: link
		};
	}]);
});
