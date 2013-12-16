define(['app/config/module'], function (module) {
	'use strict';

	module.controller('VolumeView', ['$scope', 'volume', function ($scope, volume) {
		$scope.view = {
			view: 'volume',
			volume: volume
		};
		$scope.volume = volume;

		//

		$scope.updateNetwork = function () {
			$scope.investigators = [];
			$scope.contributors = [];
			$scope.observers = [];
			$scope.viewers = [];
			$scope.funding = [];

			angular.forEach($scope.volume.access, function (partyAccess) {
				switch(partyAccess.access) {
					case 4:
						$scope.investigators.push(partyAccess.party);
						break;
					case 3:
						$scope.contributors.push(partyAccess.party);
						break;
					case 2:
						$scope.observers.push(partyAccess.party);
						break;
					case 1:
						$scope.viewers.push(partyAccess.party);
						break;
				}
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
