define(['config/module'], function (module) {
	'use strict';

	module.directive('userPasswordForm', ['authService', 'pageService', '$http', '$window', function (auth, page, $http, $window) {
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
						page.messages.add({
							type: 'green',
							countdown: 3000,
							body: page.constants.message('reset.request.success', form.data.email)
						});

						if (angular.isFunction(form.resetSuccessFn))
							form.resetSuccessFn(form, arguments);
					})
					.error(function (errors, status) {
						page.messages.addError({
							closeable: true,
							body: page.constants.message('reset.request.error'),
							errors: errors,
							status: status
						});

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
						page.messages.add({
							type: 'green',
							countdown: 3000,
							body: page.constants.message('reset.save.success', form.data.email)
						});

						if (angular.isFunction(form.saveSuccessFn))
							form.saveSuccessFn(form, arguments);

						$window.$play.object = null;
						auth.updateUser(data);
					})
					.error(function (errors, status) {
						page.messages.add({
							countdown: 3000,
							type: 'red',
							body: (errors['password.once'] || errors['password']).join('. ') + '.'
						});

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

			page.events.talk('userPasswordForm-init', form, $scope);
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
