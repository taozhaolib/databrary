define(['app/config/module'], function (module) {
	'use strict';

	module.directive('wizard', [function () {
		var compile = function ($element, $attrs, transclude) {
			return function ($scope, $element, $attrs) {
				$scope.steps = [];
				$scope.stepsList = {};

				$scope.currentStep = undefined;

				$scope.addStep = function (step) {
					$scope.steps.push(step);
					$scope.stepsList[step.id] = step;

					$scope.currentStep = step;
				};

				$scope.activateStep = function (step) {
					angular.forEach($scope.steps, function (thisStep) {
						thisStep.active = thisStep == step;
					});
				};

				//

				$scope.getListItemClasses = function (step) {
					var classes = [];

					if(step.active)
						classes.push('active');

					return classes;
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
