module.directive('focus', [
	'pageService', function (page) {
		var link = function ($scope, $element, $attrs) {
			if ($attrs.focus === '' || page.$parse($attrs.focus)($scope)) {
				page.$timeout(function () {
					$element[0].focus();
				}, 0);
			}
		};

		return {
			restrict: 'A',
			link: link,
		}
	}
]);
