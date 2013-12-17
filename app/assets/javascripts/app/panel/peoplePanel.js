define(['app/config/module'], function (module) {
	'use strict';

	module.controller('PeoplePanel', ['$scope', 'Volume', '$routeParams', function ($scope, Volume, $routeParams) {
		$scope.bootPanel = function () {
			if (!angular.isObject($scope.volume))
				$scope.volume = Volume.get($routeParams.id, {
					id: $routeParams.id,
					access: 'all'
				});
		};

		$scope.getVolumeRole = function (volumeAccess) {
			switch (volumeAccess.access) {
				case 4:
					return 'Investigator';
				case 3:
					return 'Contributor';
				case 2:
					return 'Observer';
				case 1:
					return 'Viewer';
				default:
					return '';
			}
		};
	}]);
});
