define(['config/module'], function (module) {
	'use strict';

	module.controller('PeoplePanel', ['$scope', 'Volume', '$routeParams', function ($scope, Volume, $routeParams) {
		$scope.refreshPanel = function () {
			$scope.enabled = angular.isArray($scope.volume.access) && $scope.volume.access.length > 0;
		};

		//

		$scope.notFunded = function (object) {
			return !object.funding;
		};

		//

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
