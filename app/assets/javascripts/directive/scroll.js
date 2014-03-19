define(['config/module'], function (module) {
	'use strict';

	module.directive('scroll', ['$timeout', function ($timeout) {
		var link = function ($scope, $element, $attrs) {
			var timer = $timeout(angular.noop);
			var timeout = $.isNumeric($attrs.scrollTimeout) ? parseInt($attrs.scrollTimeout) : 100;

			$element.on('resize scroll', function ($event) {
				$timeout.cancel(timer);
				timer = $timeout(function () {
					$scope.$eval($attrs.scroll, {
						$scroll: {
							$event: $event,
							$scope: $scope,
							$element: $element,
							$attrs: $attrs
						}
					});
				}, timeout);
			});
		};

		return {
			restrict: 'A',
			link: link
		};
	}]);
});
