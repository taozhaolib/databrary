define(['config/module'], function (module) {
	'use strict';

	module.controller('ExcerptsPanel', ['$scope', function ($scope) {
		$scope.bootPanel = function () {
			$scope.current = $scope.volume.assets[0] || undefined;
		};

//		$scope.refreshPanel = function () {
//			$scope.enabled = angular.isArray($scope.volume.assets) && $scope.volume.assets.length > 0;
//		};

		//

		$scope.setCurrent = function (asset) {
			$scope.current = asset;
		};

		$scope.getAssetClasses = function (asset) {
			return {
				'active': $scope.currentAsset == asset
			};
		};

		$scope.getMimeGroup = function (asset) {
			var mimetype = asset.format ? asset.format.mimetype : asset.asset.format.mimetype,
				type = mimetype.split('/')[0];

			return type == 'text' ? mimetype[1] : type;
		};
	}]);
});
