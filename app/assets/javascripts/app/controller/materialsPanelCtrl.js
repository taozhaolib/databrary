define(['app/config/module'], function (module) {
	'use strict';

	module.controller('MaterialsPanelCtrl', ['$scope', '$http', 'MessageService', function ($scope, $http, messageService) {
		$scope.currentAsset = $scope.currentAsset || undefined;

		$scope.selectAsset = function (asset) {
			$scope.currentAsset = asset;
		};
	}]);
});
