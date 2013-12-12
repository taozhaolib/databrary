define(['app/config/module'], function (module) {
	'use strict';

	module.directive('frame', [function () {
		var compile = function ($element, $attrs, transclude) {
			return function ($scope, $element, $attrs) {

				//

				var start = function () {};

				start();
			};
		};

		return {
			restrict: 'E',
			scope: true,
			templateUrl: 'figure.html',
			transclude: true,
			replace: true,
			compile: compile
		};
	}]);
});
