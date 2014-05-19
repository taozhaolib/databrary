module.directive('idle', [
	'pageService', function (page) {
		var link = function ($scope, $element, $attrs) {
			var after = $attrs.idleFor ? parseInt($attrs.idleFor) : 1000;
			var timeout;

			var fn = function () {
				$scope.$apply($attrs.idle);
			};

			$element.bind('input', function () {
				page.$timeout.cancel(timeout);
				timeout = page.$timeout(fn, after);
			});

			$element.on('$destroy', function () {
				page.$timeout.cancel(timeout);
			});
		};

		return {
			restrict: 'A',
			link: link
		};
	}
]);
