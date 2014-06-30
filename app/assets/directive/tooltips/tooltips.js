module.directive('tooltips', [
	'pageService', function (page) {
		var controller = ['$scope', '$element', '$attrs', function ($scope, $element, $attrs) {
			var Region = function () {
//				$scope.tooltips = this;
				// TODO: does this work???
				this.enabled = true;

				//

				this.getControllerClasses = function () {
					var classes = [];

					if ($scope.enabled) {
						classes.push('tooltips-enabled');
					}

					return classes;
				};

				//

				this.getTooltipClasses = function (tooltip) {
					var classes = tooltip.cls.split(' ');

					classes.push('tooltip');
					classes.push('tooltip-' + tooltip.type);

					if (tooltip.position) {
						classes.push('tooltip-' + tooltip.position[0]);
						classes.push('tooltip-' + tooltip.position[1]);
					}

					if (tooltip.visible) {
						classes.push('tooltip-visible');
					}

					return classes;
				};
			};

			Region.prototype = page.tooltips;

			return new Region();
		}];

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