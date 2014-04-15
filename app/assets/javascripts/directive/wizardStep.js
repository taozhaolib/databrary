module.directive('wizardStep', [
	function () {
		var compile = function ($element, $attrs, transclude) {
			return function ($scope, $element, $attrs) {
				$scope.id = $attrs.id;
				$scope.name = $attrs.name;
				$scope.description = $attrs.description;

				$scope.active = false;

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
	}
]);
