define(['app/config/module'], function (module) {
	'use strict';

	module.directive('portrait', [function () {
		var compile = function ($element, $attrs, transclude) {
			return function ($scope, $element, $attrs) {
				$scope.avatar = $attrs.avatar !== false;
				$scope.info = $attrs.info !== false;
				$scope.name = $attrs.name !== false;
				$scope.institution = $attrs.institution !== false;

				$scope.extras = $attrs.extras;
				$scope.links = $attrs.links;

				$scope.caption = $attrs.caption;
			};
		};

		return {
			restrict: 'E',
			templateUrl: 'portrait.html',
			replace: true,
			compile: compile
		};
	}]);
});
