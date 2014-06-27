module.directive('displayAge', [
	'pageService', function (page) {
		var link = function ($scope) {
			$scope.change = page.display.toggleAge;

			var formatAge = function () {
				$scope.age = page.display.formatAge($scope.value);
			};

			formatAge();

			page.events.listen($scope, 'displayService-toggleAge', function () {
				formatAge();
			});
		};

		return {
			restrict: 'E',
			templateUrl: 'displayAge.html',
			replace: true,
			scope: {
				value: '@'
			},
			link: link
		};
	}
]);
