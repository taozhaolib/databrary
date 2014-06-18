module.directive('validator', [
	'pageService', function (page) {
		var pre = function ($scope, $element, $attrs) {
			$scope.validator = {};
			$scope.validator.label = $attrs.label ? page.$parse($attrs.label)($scope) : undefined;
		};

		var post = function ($scope, $element, $attrs) {
			var validator = $scope.validator;

			validator.form = $scope[$attrs.form];
			validator.name = $scope[$attrs.form][$attrs.name];
			validator.$element = $element.find('[name="' + $attrs.name + '"]').first();
			validator.changed = false;
			validator.focus = false;
			validator.serverErrors = [];
			validator.clientErrors = [];
			validator.clientTips = [];

			validator.$element.focus(function () {
				$scope.$apply(function () {
					validator.focus = true;
				});
			}).blur(function () {
				$scope.$apply(function () {
					validator.focus = false;
				});
			});

			validator.iconClasses = function () {
				var cls = [];

				if (!validator.name) {
					return cls;
				}

				if (validator.name.$dirty) {
					cls.push('show');
				}

				if (validator.name.$valid) {
					cls.push('valid');
				} else {
					cls.push('invalid');
				}

				return cls;
			};

			//

			validator.show = function () {
				return validator.showClientErrors() || validator.showClientTips() || validator.showServerErrors();
			};

			validator.showServerErrors = function () {
				return validator.serverErrors.length > 0 && !validator.changed;
			};

			validator.showClientErrors = function () {
				return validator.clientErrors.length > 0 && validator.name.$invalid && validator.focus;
			};

			validator.showClientTips = function () {
				return validator.clientTips.length > 0 && !validator.name.$invalid && validator.focus;
			};

			//

			var changeWatch = function () {
				validator.changed = true;
				validator.$element.off('keypress.validator');
				validator.name.$setValidity('serverResponse', true);
			};

			validator.server = function (data, replace) {
				if (replace !== false) {
					validator.changed = false;
					validator.serverErrors = [];
					validator.$element.off('keypress.validator');
					validator.name.$setValidity('serverResponse', true);
				}

				if (!data) {
					return;
				}

				validator.$element.on('keypress.validator', changeWatch);
				validator.name.$setValidity('serverResponse', false);

				if (angular.isString(data)) {
					data = [data];
				}

				angular.forEach(data, function (error) {
					validator.serverErrors.push(error);
				});
			};

			validator.client = function (data, replace) {
				if (replace) {
					validator.clientErrors = [];
					validator.clientTips = [];
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
						validator.clientErrors.push(error);
					});
				}

				if (angular.isArray(data.tips)) {
					angular.forEach(data.tips, function (tip) {
						validator.clientTips.push(tip);
					});
				}
			};

			//

			if (validator.form && validator.form.validator) {
				validator.form.validator.add($attrs.name, validator);
			}

			if (validator.name) {
				validator.name.validator = validator;
			}
		};

		//

		return {
			restrict: 'E',
			replace: true,
			scope: true,
			transclude: true,
			templateUrl: 'validator.html',
			link: {
				pre: pre,
				post: post,
			},
		};
	}
]);
