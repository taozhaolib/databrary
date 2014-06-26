module.directive('authGrantForm', [
	'pageService', function (page) {
		var link = function ($scope, $element, $attrs) {
			var form = $scope.authGrantForm;

			form.party = page.$parse($attrs.party)($scope) || undefined;
			form.other = page.$parse($attrs.other)($scope) || undefined;

			//

			form.init = function (data) {
				form.other = data;
				form.other.member = form.other.member || 0;
				form.other.site = form.other.site || 0;
			};

			form.presetName = function (type, name, party) {
				return '<strong>' + page.constants.message('auth.' + type + '.' + name + '.title') + '</strong>: ' + page.$filter('possessive')('auth.' + type + '.' + name, party);
			};

			//

			form.saveFn = undefined;
			form.successFn = undefined;
			form.errorFn = undefined;

			form.save = function () {
				if (!form.other.expires) {
					delete form.other.expires;
				} else {
					form.other.expires = page.$filter('date')(form.other.expires, 'yyyy-MM-dd');
				}

				if (angular.isFunction(form.saveFn)) {
					form.saveFn(form);
				}

				page.models.PartyAuthorize.save({
					id: form.party.id,
					partyId: form.other.party.id
				}, form.other, function () {
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
				if (angular.isFunction(form.denyFn)) {
					form.denyFn(form);
				}

				page.models.PartyAuthorize.delete({
					id: form.party.id,
					partyId: form.other.party.id
				}, {}, function () {
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

				form.other.site = 0;
				form.other.member = 0;
			};

			//

			form.validator.client({
				expires: {
					tips: page.constants.message('auth.grant.expires.help'),
					errors: page.constants.message('auth.grant.expires.error'),
				}
			}, true);

			//

			page.events.talk('authGrantForm-init', form, $scope);
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

