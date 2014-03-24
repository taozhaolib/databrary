define(['config/module'], function (module) {
	'use strict';

	module.controller('OverviewVolumePanel', ['$scope', function ($scope) {
		$scope.refreshPanel = function () {
			$scope.enabled = angular.isObject($scope.volume);
		};

		$scope.isFunding = function (volumeAccess) {
			return !!volumeAccess.funding;
		};
	}]);
});
