define(['app/config/module'], function (module) {
	'use strict';

	module.controller('OverviewVolumePanel', ['$scope', 'Volume', '$routeParams', function ($scope, Volume, $routeParams) {
		$scope.bootPanel = function () {
			if (!$scope.volume)
				Volume.get({
					id: $routeParams.id,
					funding: 'all'
				}, function (volume) {
					$scope.party = volume;
				});
		};
	}]);
});
