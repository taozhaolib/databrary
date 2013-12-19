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

			$scope.$watch('volume', function () {
				$scope.automatePanel();
			}, true);
		};

		$scope.automatePanel = function () {
			$scope.enabled = angular.isObject($scope.volume);
		};

		//

		$scope.onModeEdit = function () {
			$scope.formReset();
		};

		//

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
