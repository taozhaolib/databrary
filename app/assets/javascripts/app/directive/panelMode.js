define(['app/config/module'], function (module) {
	'use strict';

	module.directive('panelMode', ['PanelService', function (panelService) {
		var compile = function ($element, $attrs, transclude) {
			return function ($scope, $element, $attrs) {

				var start = function () {
					$scope.name = angular.isString($attrs.name) ? $attrs.name : 'view';
					$scope.level = angular.isString($attrs.level) ? $attrs.level : $scope.name;
					$scope.enabled = angular.isDefined($attrs.disabled) ? false : true;
					$scope.active = angular.isDefined($attrs.active) ? true : false;

					$scope.modes.add($scope);

					if ($scope.active)
						$scope.activateMode($scope);

					transclude($scope, function ($clone) {
						$element.append($clone.replaceWith($clone.text()));
					});
				};

				start();
			};
		};

		return {
			restrict: 'E',
			replace: true,
			transclude: true,
			templateUrl: 'panelMode.html',
			scope: true,
			compile: compile
		};
	}]);
});
