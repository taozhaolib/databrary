define(['app/config/module'], function (module) {
	'use strict';

	module.controller('OverviewVolumePanel', ['$scope', 'Volume', '$routeParams', function ($scope, Volume, $routeParams) {
		$scope.refreshPanel = function () {
			$scope.enabled = angular.isObject($scope.volume);
		};

		//

		$scope.onModeEdit = function () {
			$scope.formReset();
		};

		$scope.formSave = function () {

		};

		$scope.formReset = function () {
			$scope.volumeForm = {
				name: $scope.volume.name,
				body: $scope.volume.body
			};
		}
	}]);
});
