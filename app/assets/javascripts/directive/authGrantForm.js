define(['config/module'], function (module) {
	'use strict';

	module.directive('authGrantForm', ['PartyAuthorize', 'AuthService', 'EventService', 'AuthPresetService', '$filter', 'Page', function (PartyAuthorize, authService, eventService, authPresetService, $filter, page) {
		var link = function ($scope) {
			var form = $scope.authGrantForm;

			form.presets = authPresetService;
			form.party = $scope.party || authService.user;
			form.other = undefined;

			//

			form.saveFn = undefined;
			form.successFn = undefined;
			form.errorFn = undefined;

			form.save = function () {
				form.partyAuthorize = new PartyAuthorize();

				form.partyAuthorize.direct = form.other.direct;
				form.partyAuthorize.inherit = form.other.inherit;
				form.partyAuthorize.expires = form.other.expiration;

				if (!form.partyAuthorize.expires.match(/^\d{4}-\d{1,2}-\d{1,2}$/))
					form.partyAuthorize.expires = '';

				if(form.partyAuthorize.expires == '')
					delete form.partyAuthorize.expires;

				if (angular.isFunction(form.saveFn))
					form.saveFn(form);

				form.partyAuthorize.$save({
					id: form.party.id,
					partyId: form.other.party.id
				}, function () {
					if (angular.isFunction(form.successFn))
						form.successFn(form, arguments);
				}, function (res) {
					page.messages.addError({
						body: page.constants.message('auth.grant.save.error'),
						errors: res[0],
						status: res[1]
					});

					if (angular.isFunction(form.errorFn))
						form.errorFn(form, arguments);
				});
			};

			//

			form.denyFn = undefined;
			form.denySuccessFn = undefined;
			form.denyErrorFn = undefined;

			form.deny = function () {
				form.partyAuthorize = new PartyAuthorize();

				if (angular.isFunction(form.denyFn))
					form.denyFn(form);

				form.partyAuthorize.$delete({
					id: form.party.id,
					partyId: form.other.party.id
				}, function () {
					if (angular.isFunction(form.denySuccessFn))
						form.denySuccessFn(form, arguments);
				}, function (res) {
					page.messages.addError({
						body: page.constants.message('auth.grant.deny.error'),
						errors: res[0],
						status: res[1]
					});

					if (angular.isFunction(form.denyErrorFn))
						form.denyErrorFn(form, arguments);
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

			var parsePresets = function () {
				var custom = undefined,
					presets = authPresetService.get(form.party, form.other);

				form.other.preset = undefined;

				angular.forEach(presets, function (preset) {
					if (form.other.direct == preset.direct && form.other.inherit == preset.inherit)
						authPresetService.set(form.other, preset);

					if (angular.isUndefined(preset.inherit))
						custom = preset;
				});

				if (angular.isUndefined(form.other.preset))
					authPresetService.set(form.other, custom);
			};

			//

			eventService.talk('authGrantForm-init', form, $scope);

			$scope.$watch('authGrantForm.other', function (newVal, oldVal) {
				parsePresets();

				if (form.other.expires)
					form.other.expiration = $filter('date')(new Date(form.other.expires), 'yyyy-MM-dd');
				else
					form.other.expiration = '';

				$scope.$watch(function () {
					return form.other.expiration;
				}, function (newVal, oldVal) {
					var now = new Date(),
						limit = new Date(now.setYear(now.getFullYear() + 2)).getTime(),
						exp = form.other.expiration.split('-'),
						trial = new Date(exp[1] + '-' + exp[2] + '-' + exp[0]).getTime();

					if (trial > limit)
						form.other.expiration = $filter('date')(limit, 'yyyy-MM-dd');
				});
			});
		};

		//

		return {
			restrict: 'E',
			templateUrl: 'authGrantForm.html',
			scope: false,
			replace: true,
			link: link
		};
	}]);
});
