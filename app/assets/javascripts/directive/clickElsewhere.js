define(['config/module'], function (module) {
	'use strict';

	module.directive('clickElsewhere', [function () {
		var link = function ($scope, $element, $attrs) {
			$element.bind('click', function ($event) {
					$event.stopPropagation();
				});

				$document.bind('click', function () {
					$scope.$apply($attrs.clickElsewhere);
				});
		};

		return {
			restrict: 'A',
			link: link
		}
	}]);
});
