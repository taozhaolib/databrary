define(['config/module'], function (module) {
	'use strict';

	module.directive('displayAge', ['EventService', 'DisplayService', function (events, display) {
		var link = function ($scope, $element, $attrs) {
			$scope.change = display.toggleAge;

			var formatAge = function () {
				$scope.age = display.formatAge($scope.value);
			};

			formatAge();

			events.listen($scope, 'DisplayService-toggleAge', function (event, format) {
				formatAge();
			})
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
