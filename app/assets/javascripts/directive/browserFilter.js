module.directive('browserFilter', [
	'pageService', function (page) {
		var link = function ($scope) {
			$scope.sortClasses = function (sort) {
				var classes = [];

				classes.push('on');
				classes.push(page.browser.isSortToggle(sort) ? 'expanded' : '');

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
