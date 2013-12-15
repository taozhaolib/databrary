define(['app/config/module'], function (module) {
	'use strict';

	module.directive('frame', [function () {
		var compile = function ($element, $attrs, transclude) {
			return function ($scope, $element, $attrs) {
				var start = function () {
					$scope.id = (angular.isDefined($attrs.id)) ? $attrs.id : '';
					$scope.title = (angular.isDefined($attrs.title)) ? $attrs.title : '';

					transclude($scope, function ($clone) {
						$element.find('[frame-body]').append($clone);
					});
				};

				start();
			};
		};

		return {
			restrict: 'A',
			templateUrl: 'frame.html',
			scope: true,
			transclude: true,
			replace: true,
			priority: 100,
			compile: compile
		};
	}]);
});
