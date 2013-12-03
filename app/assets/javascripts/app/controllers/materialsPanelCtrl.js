define([
	'app/modules/dbControllers',
	'app/services/messageService'
], function (db) {
	'use strict';

	db.controller('MaterialsPanelCtrl', ['$scope', '$http', 'MessageService', function ($scope, $http, messageService) {
		$scope.currentAsset = $scope.currentAsset || undefined;

		$scope.selectAsset = function (asset) {
			$scope.currentAsset = asset;
		};
	}]);
});
