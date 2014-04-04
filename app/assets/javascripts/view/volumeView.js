define(['config/module'], function (module) {
	'use strict';

	module.controller('VolumeView', ['$scope', 'volume', 'panelService', 'pageService', function ($scope, volume, panelService, page) {
		$scope.volume = volume;
		page.title = volume.name;

		$scope.browser.initialize('volume', volume);

		$scope.$watch('volume', function () {
			panelService.refreshPanels();
		}, true);
	}]);
});
