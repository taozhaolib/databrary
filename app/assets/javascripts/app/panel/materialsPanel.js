define(['app/config/module'], function (module) {
	'use strict';

	module.controller('MaterialsPanel', ['$scope', 'Volume', '$routeParams', function ($scope, Volume, $routeParams) {
		$scope.bootPanel = function () {
			$scope.volume = Volume.get($routeParams.id, {
				id: $routeParams.id,
				assets: 'all'
			});
		};

		//

		$scope.currentAsset = $scope.currentAsset || undefined;

		$scope.selectAsset = function (asset) {
			$scope.currentAsset = asset;
		};

		$scope.getAssetClasses = function (asset) {
			return {
				'active': $scope.currentAsset == asset.sourceId
			};
		};
	}]);
});
