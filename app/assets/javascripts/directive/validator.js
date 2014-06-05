module.directive('validator', [
	'pageService', function (page) {
		var controller = function ($scope, $element, $attrs) {
			if (!$attrs.form || !$attrs.name || !$scope[$attrs.form] || !$scope[$attrs.form][$attrs.name]) {
				return;
			}

			var that = this;

			this.form = $scope[$attrs.form];
			this.name = $scope[$attrs.form][$attrs.name];
			// TODO: won't work with repeaters
			this.$element = this.form.$element.find('[name="' + $attrs.name + '"]').first();
			this.changed = false;
			this.focus = false;
			this.serverErrors = [];
			this.clientErrors = [];
			this.clientTips = [];

			this.$element.focus(function () {
				$scope.$apply(function () {
					that.focus = true;
				});
			}).blur(function () {
				$scope.$apply(function () {
					that.focus = false;
				});
			});

			//

			this.show = function () {
				return this.showClientErrors() || this.showClientTips() || this.showServerErrors();
			};

			this.showServerErrors = function () {
				return this.serverErrors.length > 0 && !this.changed;
			};

			this.showClientErrors = function () {
				return this.clientErrors.length > 0 && this.name.$invalid && this.focus;
			};

			this.showClientTips = function () {
				return this.clientTips.length > 0 && this.name.$pristine && this.focus;
			};

			//

			var changeWatch = function () {
				that.changed = true;
				that.$element.off('change.validator');
			};

			this.server = function (data, replace) {
				if (replace) {
					this.serverErrors = [];
					this.$element.off('change.validator');
				}

				if (!data) {
					return;
				}

				this.$element.on('change.validator', changeWatch);

				if (angular.isString(data)) {
					data = [data];
				}

				angular.forEach(data, function (error) {
					that.serverErrors.push(error);
				});
			};

			this.client = function (data, replace) {
				if (replace) {
					this.clientErrors = [];
					this.clientTips = [];
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
						that.clientErrors.push(error);
					});
				}

				if (angular.isArray(data.tips)) {
					angular.forEach(data.tips, function (tip) {
						that.clientTips.push(tip);
					});
				}
			};

			//

			if ($scope[$attrs.form] && $scope[$attrs.form].validator) {
				$scope[$attrs.form].validator.add($attrs.name, this);
			}
		};

		//

		return {
			restrict: 'E',
			replace: true,
			scope: true,
			templateUrl: 'validator.html',
			controller: controller,
			controllerAs: 'validator',
		};
	}
]);
