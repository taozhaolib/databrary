define([
	'app/modules/dbControllers',
	'app/services/messageService'
], function (db) {
	'use strict';

	db.controller('LoginPanelCtrl', ['$scope', '$http', 'MessageService', function ($scope, $http, messageService) {
		$scope.method = 1;

		$scope.switchMethod = function (method) {
			$scope.method = method;
		};
	}]);
});
