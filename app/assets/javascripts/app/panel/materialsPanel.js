define(['app/config/module'], function (module) {
	'use strict';

	module.controller('MaterialsPanel', ['$scope', 'Volume', '$routeParams', function ($scope, Volume, $routeParams) {
		$scope.refreshPanel = function () {
			$scope.enabled = angular.isArray($scope.volume.assets) && $scope.volume.assets.length > 0;
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

		$scope.getMimeGroup = function (asset) {
			var group = asset.format ? asset.format.mimetype.split('/').unshift() : asset.asset.format.mimetype.split('/').unshift();

			return group;
		}
	}]);
});
