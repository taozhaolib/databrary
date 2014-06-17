module.directive('validator', [
	'pageService', function (page) {
		var link = function ($scope, $element, $attrs) {
			$scope.validator = {};

			$scope.validator.form = $scope[$attrs.form];
			$scope.validator.name = $scope[$attrs.form][$attrs.name];
			$scope.validator.$element = $element.find('[name="' + $attrs.name + '"]').first();
			$scope.validator.changed = false;
			$scope.validator.focus = false;
			$scope.validator.serverErrors = [];
			$scope.validator.clientErrors = [];
			$scope.validator.clientTips = [];

			$scope.validator.$element.focus(function () {
				$scope.$apply(function () {
					$scope.validator.focus = true;
				});
			}).blur(function () {
				$scope.$apply(function () {
					$scope.validator.focus = false;
				});
			});

			$scope.validator.iconClasses = function () {
				var cls = [];

				if (!$scope.validator.name) {
					return cls;
				}

				if ($scope.validator.name.$dirty) {
					cls.push('show');
				}

				if ($scope.validator.name.$valid) {
					cls.push('valid');
				} else {
					cls.push('invalid');
				}

				return cls;
			};

			//

			$scope.validator.show = function () {
				return $scope.validator.showClientErrors() || $scope.validator.showClientTips() || $scope.validator.showServerErrors();
			};

			$scope.validator.showServerErrors = function () {
				return $scope.validator.serverErrors.length > 0 && !$scope.validator.changed;
			};

			$scope.validator.showClientErrors = function () {
				return $scope.validator.clientErrors.length > 0 && $scope.validator.name.$invalid && $scope.validator.focus;
			};

			$scope.validator.showClientTips = function () {
				return $scope.validator.clientTips.length > 0 && $scope.validator.name.$pristine && $scope.validator.focus;
			};

			//

			var changeWatch = function () {
				$scope.validator.changed = true;
				$scope.validator.$element.off('change.validator');
			};

			$scope.validator.server = function (data, replace) {
				if (replace) {
					$scope.validator.serverErrors = [];
					$scope.validator.$element.off('change.validator');
				}

				if (!data) {
					return;
				}

				$scope.validator.$element.on('change.validator', changeWatch);

				if (angular.isString(data)) {
					data = [data];
				}

				angular.forEach(data, function (error) {
					$scope.validator.serverErrors.push(error);
				});
			};

			$scope.validator.client = function (data, replace) {
				if (replace) {
					$scope.validator.clientErrors = [];
					$scope.validator.clientTips = [];
				}

				if (!data) {
					return;
				}

				if (angular.isArray(data)) {
					data = {
						errors: data,
					};
				}

				if (angular.isString(data.errors)) {
					data.errors = [data.errors];
				}

				if (angular.isString(data.tips)) {
					data.tips = [data.tips];
				}

				if (angular.isArray(data.errors)) {
					angular.forEach(data.errors, function (error) {
						$scope.validator.clientErrors.push(error);
					});
				}

				if (angular.isArray(data.tips)) {
					angular.forEach(data.tips, function (tip) {
						$scope.validator.clientTips.push(tip);
					});
				}
			};

			//

			if ($scope[$attrs.form] && $scope[$attrs.form].validator) {
				$scope[$attrs.form].validator.add($attrs.name, $scope.validator);
			}
		};

		//

		return {
			restrict: 'E',
			replace: true,
			scope: true,
			transclude: true,
			templateUrl: 'validator.html',
			link: link,
		};
	}
]);
