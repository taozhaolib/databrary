define(['config/module'], function (module) {
	'use strict';

	module.controller('OverviewVolumePanel', ['$scope', 'ConstantService', function ($scope, constants) {
		$scope.refreshPanel = function () {
			$scope.enabled = angular.isObject($scope.volume);
		};

		$scope.isShared = function (volumeAccess) {
			return [1,2].indexOf(volumeAccess.access) > -1;
		};

		$scope.isFunding = function (volumeAccess) {
			return !!volumeAccess.funding;
		};

		$scope.shareMessage = function (volumeAccess) {
			return volumeAccess.access == 1 ? constants.message('auth.permission.VIEW', volumeAccess.party.name) : constants.message('auth.permission.DOWNLOAD', volumeAccess.party.name);
		};
	}]);
});
