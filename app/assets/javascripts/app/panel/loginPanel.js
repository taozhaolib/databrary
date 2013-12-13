define(['app/config/module'], function (module) {
	'use strict';

	module.controller('LoginPanel', ['$scope', 'AuthService', function ($scope, authService) {
		$scope.method = 'databrary';

		$scope.loginData = {};

		//

		$scope.switchMethod = function (method) {
			$scope.method = method;
		};

		$scope.showMethodLink = function (method) {
			return $scope.method != method;
		};

		$scope.getMethod = function () {
			return $scope.method;
		};

		//

		$scope.submitForm = function () {
			authService.login($scope.loginData);
		};

		//

		var start = function () {

		};

		start();
	}]);
});
