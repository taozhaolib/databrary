define(['app/config/module'], function (module) {
	'use strict';

	module.controller('DataPanel', ['$scope', 'Party', '$routeParams', '$filter', function ($scope, Party, $routeParams, $filter) {
		$scope.bootPanel = function () {
//			if (!$scope.party || !$scope.party.volumes || !$scope.party.funding)
//				Party.get({
//					id: $routeParams.id,
//					volumes: '',
//					funding: ''
//				}, function (party) {
//					$scope.party = party;
//					$scope.updateVolumes();
//				});
		};

		$scope.updateVolumes = function () {
			$scope.funded = [];
			$scope.investigated = [];
			$scope.contributed = [];

			angular.forEach($scope.party.funding, function (volumeAccess) {
				$scope.investigated.push(volumeAccess.volume);
			});

			angular.forEach($scope.party.volumes, function (volumeAccess) {
				if (volumeAccess.access == 4)
					$scope.investigated.push(volumeAccess.volume);

				if (volumeAccess.access == 3)
					$scope.contributed.push(volumeAccess.volume);
			});
		};

		$scope.$watch('party', function () {$scope.updateVolumes();}, true)
	}]);
});
