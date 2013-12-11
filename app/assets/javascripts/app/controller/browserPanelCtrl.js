define(['app/config/module'], function (module) {
	'use strict';

	module.controller('BrowserPanelCtrl', ['$scope', '$http', 'MessageService', function ($scope, $http, messageService) {
		$scope.hidePrivate = true;

		$scope.showPrivate = function () {
			$scope.hidePrivate = false;
		}
	}]);
});
