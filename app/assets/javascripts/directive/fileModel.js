module.directive('fileModel', [
	'pageService', function (page) {
		var link = function ($scope, $element, $attrs) {
			$element.bind('change', function () {
				$scope.$apply(function () {
					page.$parse($attrs.fileModel).assign($scope, $element[0].files);
				});
			});
		};

		return {
			restrict: 'A',
			link: link
		}
	}
]);
