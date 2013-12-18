define(['app/config/module'], function (module) {
	'use strict';

	module.controller('VolumeView', ['$scope', 'volume', function ($scope, volume) {
		$scope.view = {
			view: 'volume',
			volume: volume
		};
		$scope.volume = volume;
	}]);
});
