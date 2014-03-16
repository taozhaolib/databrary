define(['config/module'], function (module) {
	'use strict';

	module.directive('baseline', ['$timeout', function ($timeout) {
		var base = 6,
			pauseTime = 500;

		var link = function ($scope, $element, $attrs) {
			var ratio,
				ratioClass,
				timeout;

			$scope.updateBaseline = function () {
				$element.removeClass(ratioClass);

				if (isNaN($attrs.baseline)) {
					switch ($attrs.baseline) {
						case 'wide':
						case '16x9':
							ratio = .5625;
							ratioClass = 'wide';
							break;

						case 'tube':
						case '4x3':
							ratio = .75;
							ratioClass = 'tube';
							break;

						case 'square':
						case '1x1':
						default:
							ratio = 1;
							ratioClass = 'square';
							break;
					}
				} else {
					ratio = parseFloat($attrs.baseline);
				}

				$element.addClass(ratioClass);
			};

			$scope.triggerBaseline = function () {
				var width = $element.outerWidth(false),
					height;

				height = width * ratio;

				if (angular.isUndefined($attrs.absolute)) {
					height = height - (height % base);
				}

				if (height > 0)
					$element.height(height);
				else
					$element.css('height', '');

				var $img = $element.find('.media img');

				if (!$img.length > 0)
					return;

				var imgWidth = $img.width(),
					imgHeight = $img.height();

				if (imgWidth >= imgHeight)
					$img.css({
						'width': 'auto',
						'height': height + 'px'
					});
				else
					$img.css({
						'width': width + 'px',
						'height': 'auto'
					});

				if(imgWidth == 0 || imgHeight == 0)
					return $scope.timeoutBaseline();
			};

			$scope.timeoutBaseline = function () {
				$timeout.cancel(timeout);
				timeout = $timeout(function () {
					$scope.triggerBaseline();
				}, pauseTime);
			};

			//

			$attrs.$observe('baseline', function () {
				$scope.updateBaseline();
				$scope.triggerBaseline();
			});

			//

			$(window).on('resize', function () {
				$scope.timeoutBaseline();
			});

			$element.on('$destroy', function () {
				$timeout.cancel(timeout);
			});

			$scope.triggerBaseline();
		};

		return {
			restrict: 'A',
			scope: true,
			priority: 150,
			link: link
		};
	}]);
});
