module.directive('validInput', [
	'pageService', function (page) {
		var pre = function ($scope, $element, $attrs) {
			if ($attrs.name && $attrs.form) {
				$element.append(page.$compile('<validator form="' + $attrs.form + '" name="' + $attrs.name + '"></validator>')($scope));
			}
		};

		var post = function ($scope, $element, $attrs) {
			var field = $scope[$attrs.form] && $scope[$attrs.form][$attrs.name];

			$scope.iconClasses = function () {
				var cls = [];

				if (field && field.$dirty) {
					cls.push('show');
				}

				if (field && field.$valid) {
					cls.push('valid');
				} else {
					cls.push('invalid');
				}

				return cls;
			};
		};

		return {
			restrict: 'E',
			templateUrl: 'validInput.html',
			transclude: true,
			replace: true,
			scope: true,
			controllerAs: 'validInput',
			link: {
				pre: pre,
				post: post,
			},
		}
	}
]);
