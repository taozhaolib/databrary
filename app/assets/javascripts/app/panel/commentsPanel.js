define(['app/config/module'], function (module) {
	'use strict';

	module.controller('CommentsPanel', ['$scope', 'AuthService', function ($scope, authService) {
		$scope.authService = authService;


	}]);
});
