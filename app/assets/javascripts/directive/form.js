module.directive('form', [
	'pageService', function (page) {
		var link = function ($scope, $element, $attrs) {
			if (angular.isDefined($attrs.messages)) {
				if (!$attrs.name) {
					return;
				} else if ($attrs.messages === 'default') {
					$scope[$attrs.name].messages = page.messages;
				} else if ($attrs.messages === 'nearest' && $scope.messages) {
					$scope[$attrs.name].messages = $scope.messages;
				}
			} else if ($scope[$attrs.name] && !$scope[$attrs.name].messages) {
				$scope[$attrs.name].messages = page.messages.region();
				page.$compile('<messages form></messages>')($scope, function ($clone, $scope) {
					$element.prepend($clone);
				});
			}
		};

		return {
			restrict: 'E',
			link: link,
		}
	}
]);
