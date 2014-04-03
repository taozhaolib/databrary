define(['config/module'], function (module) {
	'use strict';

	module.controller('ResetPanel', ['$scope', 'AuthService', 'MessageService', '$location', 'ConstantService', 'EventService', 'RouterService', function ($scope, authService, messages, $location, constants, events, router) {
		events.listen($scope, 'userPasswordForm-init', function (event, form) {
			form.resetSuccessFn = function () {
				$location.url(router.index());
			};

			form.saveSuccessFn = function () {
				$location.url(router.index());
			};

			event.stopPropagation();
		});
	}]);
});
