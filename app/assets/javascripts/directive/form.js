module.directive('form', [
	'pageService', function (page) {
		var link = function ($scope, $element, $attrs) {
			if (angular.isDefined($attrs.messageRegion)) {
				if (!$attrs.name) {
					return;
				} else if ($attrs.messageRegion === 'default') {
					$scope[$attrs.name].messageRegion = page.messages;
				} else if ($attrs.messageRegion === 'nearest' && $scope.messageRegion) {
					$scope[$attrs.name].messageRegion = $scope.messageRegion;
				}
			} else if ($scope[$attrs.name] && !$scope[$attrs.name].messageRegion) {
				$scope[$attrs.name].messageRegion = page.messages.region();
				page.$compile('<message-region form></message-region>')($scope, function ($clone, $scope) {
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
