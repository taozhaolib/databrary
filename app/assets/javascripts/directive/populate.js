module.directive('populate', [
	'$compile', function ($compile) {
		var link = function ($scope, $element, $attrs) {
			console.log($scope.populate);
			$attrs.$observe('populate', function () {
				$element.append($compile('<span>' + $attrs.populate + '</span>')($scope));
			});
		};

		return {
			restrict: 'A',
			priority: 150,
			link: link
		};
	}
]);
