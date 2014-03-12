define(['app/config/module'], function (module) {
	'use strict';

	module.directive('wizard', [function () {
		var compile = function ($element, $attrs, transclude) {
			return function ($scope, $element, $attrs) {
				$scope.steps = [];
				$scope.stepsList = {};

				$scope.addStep = function (step) {
					$scope.steps.push(step);
					$scope.stepsList[step.id] = step;
				};

				//

				transclude($scope, function ($clone) {
					$element.find('[wizard-steps]').append($clone);
				});

				$scope.retrieve()($scope);
			};
		};

		return {
			restrict: 'E',
			templateUrl: 'wizard.html',
			scope: {
				retrieve: '&'
			},
			transclude: true,
			replace: true,
			compile: compile
		};
	}]);
});
