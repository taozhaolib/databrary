define(['config/module'], function (module) {
	'use strict';

	module.controller('VolumeView', ['$scope', 'volume', 'pageService', function ($scope, volume, page) {
		$scope.volume = volume;
		page.title = volume.name;

		$scope.browser.initialize('volume', volume);

		$scope.$watchCollection('volume', function () {
			page.events.talk('panelService-refresh');
		});
	}]);
});
