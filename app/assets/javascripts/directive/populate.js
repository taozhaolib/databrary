module.directive('populate', [
	'$timeout', function ($timeout) {
		var link = function ($scope, $element, $attrs) {
			$scope.$watch('populate', function () {
				$element.html($scope.populate);
			});
		};

		return {
			restrict: 'A',
			scope: {
				populate: '='
			},
			priority: 150,
			link: link
		};
	}
]);
