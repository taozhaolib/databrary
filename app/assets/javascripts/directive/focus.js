module.directive('focus', [
	'pageService', function (page) {
		var link = function ($scope, $element, $attrs) {
			if ($attrs.focus === '' || page.$parse($attrs.focus)($scope)) {
				$element[0].focus();
			}
		};

		return {
			restrict: 'A',
			link: link
		}
	}
]);
