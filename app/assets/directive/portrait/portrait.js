module.directive('portrait', [
	function () {
		var compile = function () {
			return function ($scope, $element, $attrs) {
				$element.find('.portrait').addClass($element.attr('class'));
				$element.attr('class', '');
				$scope.avatar = $attrs.avatar !== false;
				$scope.info = $attrs.info !== false;
				$scope.caption = $attrs.caption;
			};
		};

		return {
			restrict: 'E',
			templateUrl: 'portrait.html',
			replace: true,
			compile: compile
		};
	}
]);
