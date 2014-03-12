define(['app/config/module'], function (module) {
	'use strict';

	module.directive('wizard', [function () {
		var compile = function ($element, $attrs, transclude) {
			return function ($scope, $element, $attrs) {
				$scope.linear = $attrs.hasOwnProperty('linear');

				$scope.steps = [];
				$scope.stepsList = {};
				$scope.currentStep = undefined;

				$scope.addFn = undefined;

				$scope.addStep = function (step) {
					$scope.steps.push(step);
					$scope.stepsList[step.id] = step;
					$scope.currentStep = step;

					if(angular.isFunction($scope.addFn))
						$scope.addFn();
				};

				$scope.activateStep = function (step) {
					if ($scope.isStepBlocked(step))
						return;

					angular.forEach($scope.steps, function (thisStep) {
						thisStep.active = thisStep == step;
					});
				};

				$scope.isStepBlocked = function (step) {
					return $scope.linear && !step.active && !step.allow;
				};

				//

				$scope.getListItemClasses = function (step) {
					var classes = [];

					if(step.active)
						classes.push('active');

					if(step.complete === true)
						classes.push('complete');

					if(step.complete === false)
						classes.push('incomplete');

					if($scope.isStepBlocked(step))
						classes.push('blocked');

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
