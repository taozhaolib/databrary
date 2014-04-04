define(['config/module'], function (module) {
	'use strict';

	module.directive('displayAge', ['pageService', 'displayService', function (page, display) {
		var link = function ($scope) {
			$scope.change = display.toggleAge;

			var formatAge = function () {
				$scope.age = display.formatAge($scope.value);
			};

			formatAge();

			page.events.listen($scope, 'displayService-toggleAge', function () {
				formatAge();
			});
		};

		return {
			restrict: 'E',
			templateUrl: 'displayAge.html',
			replace: true,
			scope: {
				value: '@'
			},
			link: link
		};
	}]);
});
