define(['app/config/module'], function (module) {
	'use strict';

	module.directive('portrait', [function () {
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
			templateUrl: 'portrait.html',
			transclude: true,
			replace: true,
			compile: compile
		};
	}]);
});
