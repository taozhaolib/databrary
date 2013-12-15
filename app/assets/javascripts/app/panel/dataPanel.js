define(['app/config/module'], function (module) {
	'use strict';

	module.controller('DataPanel', ['$scope', 'Party', '$routeParams', '$filter', function ($scope, Party, $routeParams, $filter) {
		$scope.bootPanel = function () {
//			if (!$scope.party || !$scope.party.volumes)
//				Party.get({
//					id: $routeParams.id,
//					volumes: 'all'
//				}, function (party) {
//					$scope.party = party;
//					$scope.updateVolumes();
//				});
		};

		$scope.updateVolumes = function () {
			$scope.investigated = [];
			$scope.contributed = [];

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
