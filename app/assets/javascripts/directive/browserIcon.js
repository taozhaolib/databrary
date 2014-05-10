module.directive('browserIcon', [
	'browserService', function (browserService) {
		var link = function ($scope) {
			$scope.browser = $scope.browser || browserService;

			//

			$scope.toggleExpand = function () {
				$scope.data = $scope.browser.setItemExpand($scope.data);
			};

			$scope.expanderClasses = function (data) {
				var classes = [];

				if (browserService.canExpand(data)) {
					classes.push('enabled');
				}

				classes.push(data.expand ? 'active' : '');

				return classes;
			};
		};

		return {
			restrict: 'E',
			scope: false,
			templateUrl: 'browserIcon.html',
			replace: true,
			link: link
		};
	}
]);
