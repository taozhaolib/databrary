define(['config/module'], function (module) {
	'use strict';

	module.directive('browserPlayer', ['BrowserService', '$compile', function (browserService, $compile) {
		var link = function ($scope, $element, $attrs) {
			$scope.browser = $scope.browser || browserService;

			var $content = $element.find('.browser_player_content').first();

			//

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
		};

		return {
			restrict: 'E',
			scope: true,
			templateUrl: 'browserPlayer.html',
			replace: true,
			priority: 100,
			link: link
		};
	}]);
});
