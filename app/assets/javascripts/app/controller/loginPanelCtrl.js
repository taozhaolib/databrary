define(['app/config/module'], function (module) {
	'use strict';

	module.controller('LoginPanelCtrl', ['$scope', '$http', 'MessageService', function ($scope, $http, messageService) {
		$scope.method = 1;

		$scope.switchMethod = function (method) {
			$scope.method = method;
		};
	}]);
});
 