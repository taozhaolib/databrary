define(['config/module'], function (module) {
	'use strict';

	module.controller('DataPanel', ['$scope', 'Party', '$routeParams', '$filter', function ($scope, Party, $routeParams, $filter) {
		$scope.bootPanel = function () {
			$scope.updateVolumes();
		};

		$scope.refreshPanel = function () {
			$scope.updateVolumes();

			$scope.enabled = $scope.funded.length > 0 || $scope.investigated.length > 0 || $scope.contributed.length > 0;
		};

		//

		$scope.updateVolumes = function () {
			$scope.funded = [];
			$scope.investigated = [];
			$scope.contributed = [];

			angular.forEach($scope.party.volumes, function (volumeAccess) {
				if (volumeAccess.access == 4)
					$scope.investigated.push(volumeAccess.volume);

				if (volumeAccess.access == 3)
					$scope.contributed.push(volumeAccess.volume);

				if (volumeAccess.funding)
					$scope.funded.push(volumeAccess.volume);
			});
		};
	}]);
});
