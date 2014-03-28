define(['config/module'], function (module) {
	'use strict';

	module.directive('userPasswordForm', ['AuthService', 'EventService', '$http', '$window', 'MessageService', 'ConstantService', '$location', 'RouterService', function (auth, events, $http, $window, messages, constants, $location, router) {
		var link = function ($scope) {
			var form = $scope.userPasswordForm;
			var token;

			form.data = {
				token: undefined,
				auth: undefined,
				email: undefined,
				password: {
					once: undefined,
					again: undefined
				}
			};

			if (auth.hasToken()) {
				token = auth.getToken();
				form.data.token = token.id;
				form.data.auth = token.auth;
			}

			//

			form.resetFn = undefined;
			form.resetSuccessFn = undefined;
			form.resetErrorFn = undefined;

			form.reset = function () {
				if (angular.isFunction(form.resetFn))
					form.resetFn(form);

				$http
					.post('/password', $scope.userPasswordForm.data)
					.success(function (data) {
						if (angular.isFunction(form.resetSuccessFn))
							form.resetSuccessFn(form, arguments);
					})
					.error(function () {
						if (angular.isFunction(form.resetErrorFn))
							form.resetErrorFn(form, arguments);
					});
			};

			//

			form.saveFn = undefined;
			form.saveSuccessFn = undefined;
			form.saveErrorFn = undefined;

			form.save = function () {
				if (angular.isFunction(form.saveFn))
					form.saveFn(form);

				$http
					.post('/api/party/' + token.party + '/password', $scope.userPasswordForm.data)
					.success(function (data) {
						if (angular.isFunction(form.saveSuccessFn))
							form.saveSuccessFn(form, arguments);

						$window.$play.object = null;
						auth.updateUser(data);
					})
					.error(function () {
						if (angular.isFunction(form.saveErrorFn))
							form.saveErrorFn(form, arguments);

						$window.$play.object = null;
					});
			};

			//

			form.ready = function () {
				if (form.data.token)
					return form.$dirty && form.$valid && form.data.password.once &&
						form.data.password.once == form.data.password.again;
				else
					return form.$dirty && form.$valid && form.data.email;
			};

			//

			events.talk('userPasswordForm-init', form, $scope);
		};

		//

		return {
			restrict: 'E',
			templateUrl: 'userPasswordForm.html',
			scope: false,
			replace: true,
			link: link
		};
	}]);
});
