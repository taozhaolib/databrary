// Module houses everything databrary

var dbModule = angular.module('DatabraryModule', []);

//

dbModule.directive('dbCarousel', function ($timeout) {
	var link = function (scope, element, attrs) {
		var children = element.children(),
			pauseTime = 5000,
			fadeTime = 1000,
			timeout;

		console.log(children);

		var update = function (reverse) {
			if (reverse !== true) {
				element.children().last().fadeOut(fadeTime, function () {
					$(this).prependTo(element).fadeIn(fadeTime);
				});
			} else {
				element.children().first().fadeOut(0, function () {
					$(this).appendTo(element).fadeIn(fadeTime);
				});
			}
		};

		var schedule = function (pause) {
			timeout = $timeout(function () {
				update(true);
				schedule(pause);
			}, pause);
		};

		element.on('$destroy', function () {
			$timeout.cancel(timeout);
		});

		schedule(pauseTime);
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

	var controller = function ($scope, $element, $attrs) {
		var folder = $element.find('[db-fold-folder]'),
			fold = $element.find('[db-fold-folded]');

		$element.addClass(foldableClass);
		folder.addClass(folderClass);
		fold.addClass(foldClass);

		$element.on('$destroy', function () {
			$element.removeClass(foldableClass+' '+currentlyClass);
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
			console.log('r');
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
		controller: controller
	}
});
