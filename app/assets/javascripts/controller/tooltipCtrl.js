module.controller('TooltipCtrl', [
	'$scope', 'pageService', function ($scope, page) {
		$scope.tooltips = page.tooltips;
		$scope.enabled = true;

		//

		$scope.getControllerClasses = function () {
			var classes = [];

			if ($scope.enabled) {
				classes.push('tooltips_enabled');
			}

			return classes;
		};

		//

		$scope.getTooltipClasses = function (tooltip) {
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
	}
]);
