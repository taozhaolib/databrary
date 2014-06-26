module.directive('fileModel', [
	'pageService', function (page) {
		var link = function ($scope, $element, $attrs) {
			$element.bind('change', function () {
				$scope.$apply(function () {
					page.$parse($attrs.fileModel).assign($scope, $element[0].files);

					if ($attrs.fileModelForm) {
						page.$parse($attrs.fileModelForm)($scope).$setDirty();
					}
				});
			});

			$scope.$watch($attrs.fileModel, function (val) {
				if (!val) {
					$element[0].value = '';

					if ($element[0].value) {
						$element[0].type = 'text';
						$element[0].type = 'file';
					}
				}
			});
		};

		return {
			restrict: 'A',
			link: link
		}
	}
]);
