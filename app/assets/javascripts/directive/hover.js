module.directive('hover', [
	'$timeout', function ($timeout) {
		var hoverableClass = 'hoverable',
			currentlyClass = 'hovered',
			hoverWrap = $('<div class="hover_wrap" style="position: relative;"></div>'),
			pauseTime = 0,
			fadeTime = 150;

		var link = function ($scope, $element) {
			var timeout, clone;

			$element.wrap(hoverWrap.clone());

			$scope.show = function () {
				var position = $element.position();

				clone = $element.clone();

				clone.hide().addClass(currentlyClass).css({
					'z-index': 750,
					'position': 'absolute',
					'left': position.left,
					'top': position.top,
					'width': '100%'
				});

				$element.after(clone);
				clone.fadeIn(fadeTime);
			};

			$scope.hide = function () {
				clone.fadeOut(fadeTime, function () {
					clone.off('mouseleave');

					clone.remove();

					clone = undefined;
				});
			};

			$element.on('mouseenter', function () {
				clearTimeout(timeout);

				$scope.show();

				clone.on('mouseleave', function () {
					clearTimeout(timeout);

					timeout = setTimeout(function () {
						$scope.hide();
					}, pauseTime);
				});
			});

			$(window).on('resize scroll', function () {
				if (typeof(clone) != 'undefined')
					$scope.hide();
			});

			$element.on('$destroy', function () {
				$timeout.cancel(timeout);
				$element.removeClass(hoverableClass);
			});

			$element.addClass(hoverableClass);
		};

		return {
			restrict: 'A',
			scope: true,
			link: link
		};
	}
]);
