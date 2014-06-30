module.directive('tooltips', [
	'pageService', function (page) {
		var controller = function ($scope, $element, $attrs) {
			var Region = function () {
//				$scope.tooltips = this;
				this.enabled = true;

				//

				this.getControllerClasses = function () {
					var classes = [];

					if ($scope.enabled) {
						classes.push('tooltips_enabled');
					}

					return classes;
				};

				//

				this.getTooltipClasses = function (tooltip) {
					var classes = tooltip.cls.split(' ');

					classes.push('tooltip');
					classes.push('tooltip_' + tooltip.type);

					if (tooltip.position) {
						classes.push('tooltip_' + tooltip.position[0]);
						classes.push('tooltip_' + tooltip.position[1]);
					}

					if (tooltip.visible) {
						classes.push('tooltip_visible');
					}

					return classes;
				};
			};

			Region.prototype = page.tooltips;

			return new Region();
		};

		//

		return {
			restrict: 'E',
			replace: true,
			templateUrl: 'tooltips.html',
			controller: controller,
			controllerAs: 'tooltips',
		};
	}
]);