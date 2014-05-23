module.controller('LoginView', [
	'$scope', 'pageService', function ($scope, page) {
		page.display.title = page.constants.message('page.title.login');

		//

		$scope.method = 'databrary';
		$scope.loginData = {};

		//

		$scope.switchMethod = function (method) {
			$scope.method = method;
		};

		$scope.showMethodLink = function (method) {
			return $scope.method != method;
		};

		//

		$scope.submitForm = function () {
			page.models.Party.login(angular.extend({
				email: '',
				password: '',
				openid: ''
			}, $scope.loginData), function (data) {
				page.auth.parseUser(data);

				if (page.auth.next) {
					page.$location.path(auth.next);
					page.auth.next = undefined;
				} else {
					page.$location.path('/');
				}
			}, function (res) {
				page.auth.parseUser(undefined);

				if(!$scope.loginForm.validator.server(res.data, true)) {
					$scope.loginForm.messages.add({
						body: page.constants.message('login.error'),
						type: 'red',
						closeable: true,
						report: res,
					});
				}
			});
		};
	}
]);
