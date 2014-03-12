define(['app/config/module'], function (module) {
	'use strict';

	module.directive('wizardStep', [function () {
		var compile = function ($element, $attrs, transclude) {
			return function ($scope, $element, $attrs) {
				transclude($scope, function ($clone) { console.log($element, $clone);
					$element.append($clone);
				});

				$scope.func = function () {};
			};
		};

		return {
			restrict: 'E',
			templateUrl: 'wizardStep.html',
			scope: false,
			transclude: true,
			replace: true,
			compile: compile
		};
	}]);
});
