define(['config/module'], function (module) {
	'use strict';

	module.controller('OverviewVolumePanel', ['$scope', 'ConstantService', '$filter', function ($scope, constants, $filter) {
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
			return constants.message('access.' + constants.data.permission[volumeAccess.access].name, volumeAccess.party.name);
		};

		$scope.ageSummary = function (summary) {
			var age = $filter('age');
			var range = age(summary.agerange[0]);

			if (summary.agerange[0] != summary.agerange[1])
				range += ' - ' + age(summary.agerange[1]);

			return constants.message('volume.ages', range, age(summary.agemean));
		};
	}]);
});
