'use strict';

module.controller('DataPanel', [
	'$scope', function ($scope) {
		$scope.refreshPanel = function () {
			$scope.enabled = $scope.volumes[0];
		}
	}
]);
