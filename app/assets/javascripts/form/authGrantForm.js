module.directive('authGrantForm', [
	'pageService', function (page) {
		var link = function ($scope) {
			var form = $scope.authGrantForm;

			var supportsDate = document.createElement('input');
			supportsDate.setAttribute('type', 'date');
			supportsDate = supportsDate.type === 'date';

			form.presets = page.authPresets;
			form.party = $scope.party || page.auth.user;
			form.other = undefined;

			//

			var dateNow = new Date(),
				dateLimit = new Date((new Date()).setYear(dateNow.getFullYear() + 2));

			form.transformExpiration = function () {
				var now = dateNow,
					limit = dateLimit.getTime(),
					exp = form.other.expiration.split('-'),
					trial = new Date(supportsDate ? exp[1] + '-' + exp[2] + '-' + exp[0] : form.other.expiration).getTime();

				if (trial > limit || isNaN(trial)) {
					form.other.expiration = page.$filter('date')(limit, 'yyyy-MM-dd');
				}

				if (trial < now.getTime()) {
					form.other.expiration = page.$filter('date')(now, 'yyyy-MM-dd');
				}
			};

			//

			form.saveFn = undefined;
			form.successFn = undefined;
			form.errorFn = undefined;

			form.save = function () {
				form.partyAuthorize = new page.models.PartyAuthorize();

				form.partyAuthorize.direct = form.other.direct;
				form.partyAuthorize.inherit = form.other.inherit;
				form.partyAuthorize.expires = form.other.expiration;

				if (!form.partyAuthorize.expires.match(/^\d{4}-\d{1,2}-\d{1,2}$/)) {
					form.partyAuthorize.expires = '';
				}

				if (form.partyAuthorize.expires == '') {
					delete form.partyAuthorize.expires;
				}

				if (angular.isFunction(form.saveFn)) {
					form.saveFn(form);
				}

				form.partyAuthorize.$save({
					id: form.party.id,
					partyId: form.other.party.id
				}, function () {
					form.validator.server({});

					if (angular.isFunction(form.successFn)) {
						form.successFn(form, arguments);
					}
				}, function (res) {
					form.validator.server(res);

					if (angular.isFunction(form.errorFn)) {
						form.errorFn(form, arguments);
					}
				});
			};

			//

			form.denyFn = undefined;
			form.denySuccessFn = undefined;
			form.denyErrorFn = undefined;

			form.deny = function () {
				form.partyAuthorize = new page.models.PartyAuthorize();

				if (angular.isFunction(form.denyFn)) {
					form.denyFn(form);
				}

				form.partyAuthorize.$delete({
					id: form.party.id,
					partyId: form.other.party.id
				}, function () {
					form.validator.server({});

					if (angular.isFunction(form.denySuccessFn)) {
						form.denySuccessFn(form, arguments);
					}
				}, function (res) {
					form.validator.server(res);

					if (angular.isFunction(form.denyErrorFn)) {
						form.denyErrorFn(form, arguments);
					}
				});
			};

			//

			form.cancelFn = undefined;

			form.cancel = function () {
				if (angular.isFunction(form.cancelFn)) {
					form.cancelFn(form);
				}

				form.other.inherit = 0;
				form.other.direct = 0;
				form.other.preset = undefined;
			};

			//

			var parsePresets = function () {
				var custom = undefined,
					presets = page.authPresets.get(form.party, form.other);

				form.other.preset = undefined;

				angular.forEach(presets, function (preset) {
					if (form.other.direct == preset.direct && form.other.inherit == preset.inherit) {
						page.authPresets.set(form.other, preset);
					}

					if (angular.isUndefined(preset.inherit)) {
						custom = preset;
					}
				});

				if (angular.isUndefined(form.other.preset)) {
					page.authPresets.set(form.other, custom);
				}
			};

			//

			page.events.talk('authGrantForm-init', form, $scope);

			$scope.$watch('authGrantForm.other', function () {
				parsePresets();

				if (form.other.expires) {
					form.other.expiration = page.$filter('date')(new Date(form.other.expires), 'yyyy-MM-dd');
				} else if (angular.isUndefined(form.other.authorized)) {
					form.other.expiration = page.$filter('date')(dateLimit, 'yyyy-MM-dd');
				} else {
					form.other.expiration = '';
				}
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
	}
]);
