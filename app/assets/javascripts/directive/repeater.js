module.directive('repeater', [
	'pageService', function (page) {
		var link = function ($scope, $el, $attrs) {
			$scope.page = page;
			$scope.repeats = [];
			$scope.template = undefined;

			$scope.addFn = undefined;
			$scope.removeFn = undefined;

			//

			$scope.remove = function (repeat, $index) {
				if (angular.isFunction($scope.removeFn)) {
					$scope.removeFn($scope, repeat, $index);
				}

				return $scope.repeats.splice($index, 1);
			};

			$scope.add = function () {
				if (angular.isFunction($scope.addFn)) {
					$scope.addFn($scope);
				}

				return $scope.repeats.push($scope.template || {});
			};

			//

			$scope.canRemove = function (repeat, $index, $first) {
				if (angular.isFunction($scope.canRemoveFn)) {
					$scope.canRemoveFn($scope, repeat, $index, $first);
				}
			};

			$scope.canAdd = function (repeat, $index, $last) {
				if (angular.isFunction($scope.canAddFn)) {
					$scope.canAddFn($scope, repeat, $index, $last);
				}
			};

			//

			$scope.retrieve()($scope);

			if (!Array.isArray($scope.repeats)) {
				$scope.repeats = [];
			}

			if ($scope.repeats.length === 0) {
				$scope.repeats.push({});
			}
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
	}
]);
