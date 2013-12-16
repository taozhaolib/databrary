define(['app/config/module'], function (module) {
	'use strict';

	module.controller('VolumeView', ['$scope', 'volume', function ($scope, volume) {
		$scope.volume = volume;
		$scope.comments = volume.comments;
	}]);
});
