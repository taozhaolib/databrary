define(['config/module'], function (module) {
	'use strict';

	module.directive('authApplyForm', ['PartyAuthorize', 'AuthService', 'EventService', 'AuthPresetService', 'ConstantService', function (PartyAuthorize, authService, eventService, authPresetService, constant) {
		var link = function ($scope) {
			var form = $scope.authApplyForm;

			$scope.constant = constant;

			form.presets = authPresetService;
			form.party = $scope.party || authService.user;
			form.other = undefined;

			//

			form.saveFn = undefined;
			form.successFn = undefined;
			form.errorFn = undefined;

			form.save = function () {
				if (angular.isFunction(form.saveFn))
					form.saveFn(form);

				form.partyAuthorize = new PartyAuthorize();

				form.partyAuthorize.direct = form.other.direct;
				form.partyAuthorize.inherit = form.other.inherit;

				form.partyAuthorize.$apply({
					id: form.party.id,
					partyId: form.other.party.id
				}, function () {
					if (angular.isFunction(form.successFn))
						form.successFn(form, arguments);
				}, function () {
					if (angular.isFunction(form.errorFn))
						form.errorFn(form, arguments);
				});
			};

			//

			form.cancelFn = undefined;

			form.cancel = function () {
				if (angular.isFunction(form.cancelFn))
					form.cancelFn(form);

				form.other.inherit = 0;
				form.other.direct = 0;
				form.other.preset = undefined;
			};

			//

			eventService.talk('authApplyForm-init', form, $scope);
		};

		//

		return {
			restrict: 'E',
			templateUrl: 'authApplyForm.html',
			scope: false,
			replace: true,
			link: link
		};
	}]);
});
