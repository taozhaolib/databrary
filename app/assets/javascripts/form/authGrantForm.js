module.directive('authGrantForm', [
	'pageService', function (page) {
		var link = function ($scope, $element, $attrs) {
			var form = $scope.authGrantForm;

			form.party = page.$parse($attrs.party)($scope) || undefined;
			form.other = page.$parse($attrs.other)($scope) || undefined;
			var backup = {};

			form.other.member = form.other.member || 0;
			form.other.site = form.other.site || 0;
			backup = $.extend(true, {}, form.other);

			//

			form.presetName = function (type, name, party) {
				return '<strong>' + page.constants.message('auth.' + type + '.' + name + '.title') + '</strong>: ' + page.$filter('possessive')('auth.' + type + '.' + name, party);
			};
			
			$scope.canGrantSite = function (p) {
				return  p <= page.constants.data.permissionName.PUBLIC ||
					p == page.constants.data.permissionName.READ ||
					p > page.constants.data.permissionName.READ &&
					page.auth.hasAccess(p+1);
			};

			$scope.canGrantMember = function (p) {
				return  p == page.constants.data.permissionName.NONE ||
					p == page.constants.data.permissionName.READ ||
					p == page.constants.data.permissionName.EDIT ||
					p == page.constants.data.permissionName.ADMIN;
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
					backup = $.extend(true, {}, form.other);
					page.models.Party.$cache.removeAll();
					form.$setPristine();

					if (angular.isFunction(form.successFn)) {
						form.successFn(form, arguments);
					}
				}, function (res) {
					form.validator.server(res);
					page.display.scrollTo(form.$element);

					if (angular.isFunction(form.errorFn)) {
						form.errorFn(form, arguments);
					}
				});
			};

			//

			form.resetFn = undefined;

			form.reset = function () {
				if (angular.isFunction(form.resetFn)) {
					form.resetFn(form);
				}

				form.validator.clearServer();
				form.other = $.extend(true, {}, backup);

				if (form.other.new) {
					form.deny();
				} else {
					form.$setPristine();
				}
			};

			//

			form.denyFn = undefined;
			form.denySuccessFn = undefined;
			form.denyErrorFn = undefined;

			form.deny = function () {
				if (angular.isFunction(form.denyFn)) {
					form.denyFn(form);
				}

				if (form.other.new) {
					if (angular.isFunction(form.denySuccessFn)) {
						form.denySuccessFn(form, arguments);
					}
				} else {
					page.models.PartyAuthorize.delete({
						id: form.party.id,
						partyId: form.other.party.id
					}, {}, function () {
						form.validator.server({});
						page.models.Party.$cache.removeAll();

						if (angular.isFunction(form.denySuccessFn)) {
							form.denySuccessFn(form, arguments);
						}
					}, function (res) {
						form.validator.server(res);
						page.display.scrollTo(form.$element);

						if (angular.isFunction(form.denyErrorFn)) {
							form.denyErrorFn(form, arguments);
						}
					});
				}
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
			link: link,
		};
	}
]);

