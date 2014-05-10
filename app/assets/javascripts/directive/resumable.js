module.directive('resumable', [
	'$timeout', function ($timeout) {
		var link = function ($scope, $element, $attrs) {
			var r = new Resumable();

		};

		return {
			restrict: 'E',
			scope: true,
			replace: true,
			templateUrl: 'resumable.html',
			link: link
		};
	}
]);
