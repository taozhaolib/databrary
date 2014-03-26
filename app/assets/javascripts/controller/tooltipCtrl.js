define(['config/module'], function (module) {
	'use strict';

	module.controller('TooltipCtrl', ['$scope', '$timeout', 'TooltipService', function ($scope, $timeout, tooltips) {
		$scope.tooltips = tooltips;
		$scope.enabled = true;

		//

		$scope.getControllerClasses = function () {
			var classes = [];

			if ($scope.enabled)
				classes.push('tooltips_enabled');

			return classes;
		};

		//

		$scope.getTooltipClasses = function (tooltip) {
			var classes = tooltip.cls.split(' ');

			classes.push('tooltip');
			classes.push('tooltip_' + tooltip.type);

			if(tooltip.position) {
				classes.push('tooltip_' + tooltip.position[0]);
				classes.push('tooltip_' + tooltip.position[1]);
			}

			if(tooltip.visible)
				classes.push('tooltip_visible');

			return classes;
		};

		//

		$scope.updateHeight = function () {
			var $window = $(window),
				$main = $('#main');

			var padding = 0;

			angular.forEach($scope.tooltips, function (tooltip) {
				if (tooltip.enabled)
					padding += $('#' + tooltip.id).outerHeight();
			});

			$window.scrollTop($window.scrollTop() + padding - parseInt($main.css('padding-top')));
			$main.css('padding-top', padding);
		};

		$scope.$watch('tooltips', function (tooltips) {
			$scope.updateHeight();
		});
	}]);
});
