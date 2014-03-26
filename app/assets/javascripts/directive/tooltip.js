define(['config/module'], function (module) {
	'use strict';

	module.directive('tooltip', ['TooltipService', function (tooltips) {
		var link = function ($scope, $element, $attrs) {
			var tooltip = {};

			tooltip.id = $attrs.tooltipId;
			tooltip.cls = $attrs.tooltipClass;
			tooltip.type = $attrs.tooltip || $attrs.tooltipType;
			tooltip.message = $attrs.tooltipMessage;
			tooltip.$target = $element;

			if (tooltip.message)
				tooltips.add(tooltip);
		};

		return {
			restrict: 'EA',
			link: link
		}
	}]);
});
