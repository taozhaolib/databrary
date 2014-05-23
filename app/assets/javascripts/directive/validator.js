module.directive('validator', [
	'pageService', function (page) {
		var controller = function ($scope, $element, $attrs) {
			if (!$attrs.form || !$attrs.name || !$scope[$attrs.form] || !$scope[$attrs.form][$attrs.name]) {
				return;
			} else if ($scope[$attrs.form] && $scope[$attrs.form].validator) {
				$scope[$attrs.form].validators[$attrs.name] = this;
			}

			this.form = $scope[$attrs.form];
			this.name = $scope[$attrs.form][$attrs.name];
			this.$element = this.form.$element.find('[name="'+$attrs.name+'"]').first(); // TODO: won't work with repeaters
			this.changed = false;
			this.serverErrors = [];
			this.clientErrors = [];
			this.clientTips = [];

			//

			this.hasFocus = function () {
				return this.$element.is(":focus");
			};

			this.showServerErrors = function () {
				return !this.changed;
			};

			this.showClientErrors = function () {
				return this.name.$invalid && this.hasFocus();
			};

			this.showClientTips = function () {
				return this.name.$pristine && this.hasFocus();
			};

			//

			this.update = function (server, client, replace) {

			};
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
