define(['app/config/module'], function (module) {
	'use strict';

	module.directive('wizardStep', [function () {
		var compile = function ($element, $attrs, transclude) {
			return function ($scope, $element, $attrs) {
				$scope.name = $attrs.name;

				transclude($scope, function ($clone) {
					$element.find('[wizard-step-body]').append($clone);
				});

				$scope.addStep($scope);
			};
		};

		return {
			restrict: 'E',
			templateUrl: 'wizardStep.html',
			scope: true,
			transclude: true,
			replace: true,
			compile: compile
		};
	}]);
});
