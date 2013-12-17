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
			$scope.funding = [];

			angular.forEach($scope.volume.funding, function (funding) {
				var funder = funding.party;
				funder.grant = funding.grant;

				$scope.funding.push(funder);
			});
		};

		$scope.$watch('volume', function () {$scope.updateNetwork();}, true);
	}]);
});
