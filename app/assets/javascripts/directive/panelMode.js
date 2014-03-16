define(['config/module'], function (module) {
	'use strict';

	module.directive('panelMode', ['PanelService', 'AuthService', function (panelService, authService) {
		var compile = function ($element, $attrs, transclude) {
			return function ($scope, $element, $attrs) {

				var start = function () {
					$scope.name = angular.isString($attrs.name) ? $attrs.name : 'view';
					$scope.access = !angular.isString($attrs.access) ? true :
						authService.hasAccess($attrs.access, $scope.party ? $scope.party : $scope.volume);
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
