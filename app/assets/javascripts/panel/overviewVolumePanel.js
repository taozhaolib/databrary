module.controller('OverviewVolumePanel', [
	'$scope', 'pageService', '$filter', function ($scope, page, $filter) {
		$scope.refreshPanel = function () {
			$scope.enabled = angular.isObject($scope.volume);
		};

		$scope.isShared = function (volumeAccess) {
			return [1, 2].indexOf(volumeAccess.access) > -1;
		};

		$scope.isFunding = function (volumeAccess) {
			return !!volumeAccess.funding;
		};

		$scope.shareMessage = function (volumeAccess) {
			return page.constants.message('access.' + page.constants.data.permission[volumeAccess.access].name, volumeAccess.party.name);
		};

		$scope.ageSummary = function (summary) {
			var age = $filter('age');
			var range = age(summary.agerange[0]);

			if (summary.agerange[0] != summary.agerange[1])
				range += ' - ' + age(summary.agerange[1]);

			return page.constants.message('volume.ages', range, age(summary.agemean));
		};

		$scope.hasProps = function (volume, property) {
			if (!volume[property])
				return false;

			var has = false;

			for (var prop in volume[property])
				if (volume[property].hasOwnProperty(prop))
					return true;

			return false;
		};
	}
]);
