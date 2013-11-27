define(['app/modules/dbDirectives'], function (db) {
	'use strict';

	db.directive('baseline', ['$timeout', function ($timeout) {
		var base = 6,
			pauseTime = 500;

		var link = function ($scope, $element, $attrs) {
			var ratio,
				timeout;

			$scope.updateRatio = function (val) {
				if ($.isNumeric(val))
					ratio = parseFloat(val);
				else
					switch (val) {
						case 'wide':
						case '16x9':
							ratio = .5625;
							break;

						case 'tube':
						case '4x3':
							ratio = .75;
							break;

						case 'square':
						case '1x1':
						default:
							ratio = 1;
							break;
					}
			};

			$scope.trigger = function () {
				var width = $element.outerWidth(false),
					height;

				height = width * ratio;
				height = height - (height % base);

				if (height > 0)
					$element.height(height);
				else
					$element.css('height', '');
			};

			$(window).on('resize', function () {
				clearTimeout(timeout);

				timeout = setTimeout(function () {
					$scope.trigger();
				}, pauseTime);
			});

			$element.on('$destroy', function () {
				$timeout.cancel(timeout);
			});

			$scope.updateRatio($attrs.dbBaselineRatio);
			$element.removeAttr('db-baseline-ratio');

			$scope.trigger();
		};

		return {
			restrict: 'A',
			scope: true,
			link: link
		};
	}]);
});
