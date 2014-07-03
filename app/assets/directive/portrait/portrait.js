module.directive('portrait', [
	function () {
		var compile = function () {
			return function ($scope, $element, $attrs) {
				$element.find('.portrait').addClass($element.attr('class'));
				$element.attr('class', '');
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
