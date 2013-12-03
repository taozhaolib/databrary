define([
	'app/modules/dbControllers',
	'app/services/eventService',
	'app/services/authService'
], function (db) {
	'use strict';

	db.controller('ToolbarCtrl', ['$scope', '$location', '$anchorScroll', '$timeout', 'EventService', 'AuthService', function ($scope, $location, $anchorScroll, $timeout, eventService, authService) {

		$scope.authUser = authService.getAuthUser();

		$scope.scrollTo = function (panel) {
			$location.hash(panel.id);
			$anchorScroll();
		};

		//

		$scope.logIn = authService.logIn;
		$scope.logOut = authService.logOut;
		$scope.isUser = authService.isUser;

		$scope.enableSU = authService.enableSU;
		$scope.disableSU = authService.disableSU;
		$scope.toggleSU = authService.toggleSU;
		$scope.isSU = authService.isSU;

		//


	}]);
});
