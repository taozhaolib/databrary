module.directive('browserFilter', ['browserService', function (browserService) {
	var link = function ($scope) {
		$scope.browser = $scope.browser || browserService;

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
}]);
