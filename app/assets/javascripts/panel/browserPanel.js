define(['config/module'], function (module) {
	'use strict';

	module.controller('BrowserPanel', ['$scope', '$route', function ($scope, $route) {
		$scope.refreshPanel = function () {
			switch($route.current.controller) {
				case 'PartyView':
					var enabled = false;

					angular.forEach($scope.party.volumes, function (access) {
						if(access.access > 3)
							enabled = true;
					});

					$scope.enabled = enabled;
					break;
			}
		};
	}]);
});
