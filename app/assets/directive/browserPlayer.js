'use strict';

module.directive('browserPlayer', [
	'pageService', function (page) {
		var link = function ($scope) {
			$scope.getAssetClasses = function (asset) {
				return {
					'active': $scope.currentAsset == asset
				};
			};

			$scope.getMimeGroup = function (asset) {
				var mimetype = page.types.assetFormat(asset).mimetype,
					type = mimetype.split('/')[0];

				return type == 'text' ? mimetype[1] : type;
			};

			$scope.supported = function () {
				return page.$window.navigator.userAgent.toLowerCase().indexOf('firefox') == -1 || page.$window.navigator.platform.toLowerCase().indexOf('mac') == -1;
			};
		};

		return {
			restrict: 'E',
			scope: true,
			templateUrl: 'browserPlayer.html',
			replace: true,
			priority: 100,
			link: link
		};
	}
]);
