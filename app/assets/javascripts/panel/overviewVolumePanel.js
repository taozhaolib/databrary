define(['config/module'], function (module) {
	'use strict';

	module.controller('OverviewVolumePanel', ['$scope', function ($scope) {
		$scope.refreshPanel = function () {
			$scope.enabled = angular.isObject($scope.volume);
		};

		$scope.isShared = function (volumeAccess) {
			return [1,2].indexOf(volumeAccess.access) > -1;
		};

		$scope.isFunding = function (volumeAccess) {
			return !!volumeAccess.funding;
		};
	}]);
});
