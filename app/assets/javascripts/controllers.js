// Module houses everything databrary

var dbModule = angular.module('DatabraryModule', []);

//

dbModule.directive('dbCarousel', function ($timeout) {
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
});

//

dbModule.directive('dbFold', function () {
	var foldableClass = 'foldable',
		folderClass = 'folder',
		foldClass = 'fold',
		currentlyClass = 'folded',
		slideTime = 500;

	var link = function ($scope, $element, $attrs) {
		var folder = $element.find('[db-fold-folder]'),
			fold = $element.find('[db-fold-folded]');

		$element.addClass(foldableClass);
		folder.addClass(folderClass);
		fold.addClass(foldClass);

		$element.on('$destroy', function () {
			$element.removeClass(foldableClass + ' ' + currentlyClass);
			folder.removeClass(folderClass);
			fold.removeClass(foldClass);
		});

		$scope.hide = function () {
			$element.addClass(currentlyClass);
			fold.slideUp(slideTime);
		};

		$scope.show = function () {
			$element.removeClass(currentlyClass);
			fold.slideDown(slideTime);
		};

		$scope.toggle = function () {
			$scope.currently = !$scope.currently;
		};

		$scope.$watch('currently', function (currently) {
			if (currently) {
				$scope.hide();
			} else {
				$scope.show();
			}
		});

		$scope.currently = $attrs.dbFoldCurrently == "true";
		$element.removeAttr('db-fold-currently');
	};

	return {
		restrict: 'A',
		scope: true,
		link: link
	}
});

//

dbModule.directive('dbBaseline', function ($timeout) {
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
});

//

dbModule.directive('dbHover', function ($timeout) {
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
			console.log('hide');
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
			if(typeof(clone) != 'undefined')
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
});

//

//dbModule.controller('dbMessageCtrl', function ($scope) {
//	$scope.messages = [];
//});
//
//dbModule.directive('dbMessage', function () {
//	return {
//		restrict: 'A',
//		link: link,
//		require: {
//
//		}
//	}
//});