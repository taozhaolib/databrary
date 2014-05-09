module.directive('repeater', ['pageService', function (page) {
	var link = function ($scope, $el, $attrs) {
		$scope.repeats = [];
		$scope.template = undefined;

		$scope.addFn = undefined;
		$scope.removeFn = undefined;

		//

		$scope.remove = function (repeat, $index) {
			if (angular.isFunction($scope.removeFn))
				$scope.removeFn($scope, repeat, $index);

			return $scope.repeats.splice($index, 1);
		};

		$scope.add = function (repeat, $index) {
			if (angular.isFunction($scope.addFn))
				$scope.addFn($scope, repeat, $index);

			return $scope.repeats.splice($index + 1, 0, $scope.template || {});
		};

		//

		$scope.canRemove = function (repeat, $index, $first) {
			return $scope.repeats.length > 1;
		};

		$scope.canAdd = function (repeat, $index, $last) {
			return $last;
		};

		//

		$scope.retrieve()($scope);
	};

	return {
		restrict: 'E',
		templateUrl: 'repeater.html',
		priority: 50,
		replace: true,
		transclude: true,
		scope: {
			retrieve: '&'
		},
		link: link,
	}
}]);
