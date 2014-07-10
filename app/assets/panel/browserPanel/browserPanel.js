'use strict';

module.controller('BrowserPanel', [
	'$scope', 'pageService', function ($scope, page) {
		$scope.refreshPanel = function () {
			switch (page.$route.current.controller) {
				case 'VolumeView':
					$scope.enabled = $scope.volume.sessions;
					break;
				case 'PartyView':
					$scope.enabled = $scope.volumes[0];
					break;
			}
		};
	}
]);
