define(['app/config/module'], function (module) {
	'use strict';

	module.controller('OverviewVolumePanel', ['$scope', 'Volume', '$routeParams', function ($scope, Volume, $routeParams) {
		$scope.bootPanel = function () {
			$scope.volume = Volume.get($routeParams.id, {
				id: $routeParams.id,
				funding: 'all',
				summary: 'all',
				access: 'all'
			});
		};
	}]);
});
