define(['app/config/module'], function (module) {
	'use strict';

	module.directive('panelMode', ['PanelService', function (panelService) {
		var link = function ($scope, $element, $attrs) {
			var start = function () {};

			start();
		};

		return {
			restrict: 'E',
			link: link
		};
	}]);
});
