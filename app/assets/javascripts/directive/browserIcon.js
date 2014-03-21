define(['config/module'], function (module) {
	'use strict';

	module.directive('browserIcon', ['BrowserService', function (browserService) {
		var link = function ($scope) {
			$scope.browser = $scope.browser || browserService;

			//

			$scope.toggleExpand = function () {
				$scope.data = $scope.browser.setItemExpand($scope.data);
			};

			$scope.expanderClasses = function (data) {
				var classes = [];

				classes.push(data.expand ? 'active' : '');

				return classes;
			};
		};

		return {
			restrict: 'E',
			scope: {
				'data': '='
			},
			templateUrl: 'browserIcon.html',
			replace: true,
			priority: 100,
			link: link
		};
	}]);
});
