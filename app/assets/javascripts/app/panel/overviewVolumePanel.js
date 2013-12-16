define(['app/config/module'], function (module) {
	'use strict';

	module.controller('OverviewVolumePanel', ['$scope', 'Volume', '$routeParams', function ($scope, Volume, $routeParams) {
		$scope.bootPanel = function () {
//			if (!$scope.volume)
//				Volume.get({
//					id: $routeParams.id,
//					funding: 'all'
//				}, function (volume) {
//					$scope.volume = volume;
//				});
		};

		//

		$scope.updateNetwork = function () {
			$scope.investigators = [];
			$scope.contributors = [];
			$scope.observers = [];
			$scope.funding = [];

			angular.forEach($scope.volume.access, function (partyAccess) {
				if (partyAccess.access == 4)
					$scope.investigators.push(partyAccess.party);

				if (partyAccess.access == 3)
					$scope.contributors.push(partyAccess.party);

				if (partyAccess.access == 2)
					$scope.observers.push(partyAccess.party);
			});

			angular.forEach($scope.volume.funding, function (funding) {
				var funder = funding.funder;
				funder.grant = funding.grant;

				$scope.funding.push(funder);
			});
		};

		$scope.$watch('volume', function () {$scope.updateNetwork();}, true);
	}]);
});
