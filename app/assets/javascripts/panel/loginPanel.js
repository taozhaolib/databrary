module.controller('LoginPanel', [
	'$scope', 'pageService', function ($scope, page) {
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
			page.auth.login($scope.loginData);
		};
	}
]);
