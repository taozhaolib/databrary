define(['app/config/module'], function (module) {
	'use strict';

	module.directive('browserList', ['BrowserService', '$filter', function (browserService) {
		var link = function ($scope, $element, $attrs) {
			if(!$scope.browser)
				$scope.browser = browserService;

			$scope.getInclude = function () {
				if ($scope.data.items[0])
					return 'browser' +
						$scope.data.items[0].type.charAt(0).toUpperCase() +
						$scope.data.items[0].type.slice(1) +
						'.html';
			};

			$scope.toggleExpand = function () {
				$scope.data = $scope.browser.setItemExpand($scope.data);
			};

			$scope.expanderClasses = function (data) {
				var classes = [];

				classes.push($scope.browser.getItemExpand(data) ? 'first' : '');

				return classes;
			};
		};

			$scope.setItemSelect = function (data) {
				$scope.browser.setItemSelect(data);
			};
		};

		return {
			restrict: 'E',
			scope: {
				'data': '='
			},
			templateUrl: 'browserList.html',
			replace: true,
			priority: 100,
			link: link
		};
	}]);
});
