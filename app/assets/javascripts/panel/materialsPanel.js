define(['config/module'], function (module) {
	'use strict';

	module.controller('MaterialsPanel', ['$scope', function ($scope) {
		$scope.bootPanel = function () {
			$scope.currentAsset = $scope.volume.assets[0] || undefined;
		};

		$scope.refreshPanel = function () {
			$scope.enabled = angular.isArray($scope.volume.assets) && $scope.volume.assets.length > 0;
		};

		//

		$scope.selectAsset = function (asset) {
			$scope.currentAsset = asset;
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

		$scope.getClassification = function (asset) {
			var brief;

			switch(asset.asset.classification) {
				case 0: // identified
					brief = 'I';
					break;

				case 1: // excerpt
					brief = 'E';
					break;

				case 2: // deidentified
					brief = 'D';
					break;

				case 3: // analysis
					brief = 'A';
					break;

				case 4: // product
					brief = 'P';
					break;

				case 5: // material
					brief = 'M';
					break;
			}

			return brief;
		};
	}]);
});
