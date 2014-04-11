define(['config/module'], function (module) {
	'use strict';

	module.controller('BrowserPanel', ['$scope', '$route', function ($scope, $route) {
		$scope.refreshPanel = function () {
			switch($route.current.controller) {
				case 'VolumeView':
					$scope.enabled = $scope.volume.sessions;
					break;
				case 'PartyView':
					$scope.enabled = $scope.volumes[0];
					break;
			}
		};
	}]);
});
