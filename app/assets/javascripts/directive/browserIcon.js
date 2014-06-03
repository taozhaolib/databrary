module.directive('browserIcon', [
	'pageService', function (page) {
		var link = function ($scope) {
			$scope.toggleExpand = function () {
				$scope.data = page.browser.setItemExpand($scope.data);
			};

			$scope.expanderClasses = function (data) {
				var classes = [];

				if (page.browser.canExpand(data)) {
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
