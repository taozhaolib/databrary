module.directive('populate', [
	'$compile', '$parse', function ($compile, $parse) {
		var link = function ($scope, $element, $attrs) {
			$attrs.$observe('populate', function () {
				var parse = $parse($attrs.populate)($scope);

				if (angular.isString(parse)) {
					$element.append($compile('<span>' + parse + '</span>')($scope));
				} else {
					$element.html(parse);
				}
			});
		};

		return {
			restrict: 'A',
			priority: 150,
			link: link
		};
	}
]);
