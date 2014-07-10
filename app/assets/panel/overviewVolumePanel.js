'use strict';

module.controller('OverviewVolumePanel', [
	'$scope', 'pageService', function ($scope, page) {
		$scope.refreshPanel = function () {
			$scope.enabled = angular.isObject($scope.volume);
		};

		$scope.isContributor = function (volumeAccess) {
			return volumeAccess.individual && volumeAccess.individual >= page.permission.ADMIN;
		};

		$scope.isShared = function (volumeAccess) {
			return volumeAccess.individual && volumeAccess.individual < page.permission.ADMIN;
		};

		$scope.shareMessage = function (volumeAccess) {
			return page.constants.message('access.' + page.constants.data.permission[volumeAccess.individual], volumeAccess.party.name);
		};

		$scope.ageSummary = function (summary) {
			var age = page.$filter('age');
			var range = age(summary.agerange[0]);

			if (summary.agerange[0] != summary.agerange[1]) {
				range += ' - ' + age(summary.agerange[1]);
			}

			return page.constants.message('volume.ages', range, age(summary.agemean));
		};

		$scope.hasProps = function (volume, property) {
			if (!volume[property]) {
				return false;
			}

			for (var prop in volume[property]) {
				if (volume[property].hasOwnProperty(prop)) {
					return true;
				}
			}

			return false;
		};
	}
]);
