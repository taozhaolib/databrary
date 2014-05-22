module.directive('form', [
	'pageService', function (page) {
		var link = function ($scope, $element, $attrs) {
			switch ($attrs.messages) {
				case 'nearest':
					if ($scope.messages) {
						$scope[$attrs.name].messages = $scope.messages;
					}
					break;

				case 'none':
					$scope[$attrs.name].messages = page.messages.region();
					break;

				case 'default':
					$scope[$attrs.name].messages = page.messages;
					break;

				default:
					if(angular.isString($attrs.messages)) {
						$scope[$attrs.name].messages = page.$parse($attrs.messages)($scope);
					}

					if (!$scope[$attrs.name].messages instanceof page.messages.constructor) {
						$scope[$attrs.name].messages = page.messages.region();
					}
					break;
			}
		};

		return {
			restrict: 'E',
			link: link,
		}
	}
]);
