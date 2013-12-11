define(['app/config/module'], function (module) {
	'use strict';

	module.directive('carousel', ['$timeout', function ($timeout) {
		var link = function ($scope, $element) {
			var pauseTime = 5000,
				fadeTime = 1000,
				timeout;

			$scope.update = function (reverse) {
				if (reverse !== true) {
					$element.children().last().fadeOut(fadeTime, function () {
						$(this).prependTo($element).fadeIn(fadeTime);
					});
				} else {
					$element.children().first().fadeOut(0, function () {
						$(this).appendTo($element).fadeIn(fadeTime);
					});
				}
			};

			$scope.schedule = function (pause) {
				timeout = $timeout(function () {
					$scope.update(true);
					$scope.schedule(pause);
				}, pause);
			};

			$element.on('$destroy', function () {
				$timeout.cancel(timeout);
			});

			$scope.schedule(pauseTime);
		};

		return {
			restrict: 'A',
			link: link
		};
	}]);
});
