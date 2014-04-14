module.directive('browserPlayer', ['browserService', '$window', function (browserService, $window) {
	var link = function ($scope, $element) {
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

		$scope.supported = function () {
			return $window.navigator.userAgent.toLowerCase().indexOf('firefox') == -1 || $window.navigator.platform.toLowerCase().indexOf('mac') == -1;
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
