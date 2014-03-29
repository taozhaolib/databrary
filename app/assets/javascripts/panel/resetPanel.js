define(['config/module'], function (module) {
	'use strict';

	module.controller('ResetPanel', ['$scope', 'AuthService', 'MessageService', '$location', 'ConstantService', 'EventService', 'RouterService', function ($scope, authService, messages, $location, constants, events, router) {
		events.listen($scope, 'userPasswordForm-init', function (event, form) {
			form.resetSuccessFn = function () {
				messages.add({
					closeable: true,
					type: 'green',
					body: constants.message('reset.request.complete', form.data.email)
				});

				$location.url(router.index());
			};

			form.saveSuccessFn = function () {
				messages.add({
					closeable: true,
					countdown: 3000,
					type: 'green',
					body: constants.message('reset.save.complete', form.data.email)
				});

				$location.url(router.index());
			};

			event.stopPropagation();
		});
	}]);
});
