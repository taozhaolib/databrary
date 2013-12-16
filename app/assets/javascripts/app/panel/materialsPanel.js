define(['app/config/module'], function (module) {
	'use strict';

	module.controller('MaterialsPanel', ['$scope', function ($scope) {
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
