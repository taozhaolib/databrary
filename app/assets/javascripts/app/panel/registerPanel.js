define(['app/config/module'], function (module) {
	'use strict';

	module.controller('RegisterPanel', ['$scope', 'AuthService', function ($scope, authService) {
		$scope.wizard = {};

		$scope.retrieveWizard = function(wizard) {
			$scope.wizard = wizard;
		};
	}]);
});
