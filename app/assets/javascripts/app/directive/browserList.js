define(['app/config/module'], function (module) {
	'use strict';

	module.directive('browserList', ['BrowserService', '$filter', function (browserService) {
		var link = function ($scope, $element, $attrs) {
			$scope.getInclude = function () {
				if ($scope.data.items[0])
					return 'browser' +
						$scope.data.items[0].type.charAt(0).toUpperCase() +
						$scope.data.items[0].type.slice(1) +
						'.html';
			}
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
