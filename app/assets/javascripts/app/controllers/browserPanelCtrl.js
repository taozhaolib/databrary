define([
	'app/modules/dbControllers',
	'app/services/messageService'
], function (db) {
	'use strict';

	db.controller('BrowserPanelCtrl', ['$scope', '$http', 'MessageService', function ($scope, $http, messageService) {
		$scope.hidePrivate = true;

		$scope.showPrivate = function () {
			$scope.hidePrivate = false;
		}
	}]);
});
