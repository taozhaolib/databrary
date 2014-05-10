module.directive('browserFilter', [
	'pageService', function (page) {
		var link = function ($scope) {
			$scope.browser = $scope.browser || page.browser;

			//

			$scope.sortClasses = function (sort) {
				var classes = [];

				classes.push('on');
				classes.push($scope.browser.isSortToggle(sort) ? 'expanded' : '');

				return classes;
			};
		};

		return {
			restrict: 'E',
			scope: true,
			templateUrl: 'browserFilter.html',
			replace: true,
			priority: 100,
			link: link
		};
	}
]);
