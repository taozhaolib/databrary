module.directive('accessGrantForm', [
	'pageService', function (page) {
		var link = function ($scope, $element, $attrs) {
			var form = $scope.accessGrantForm;
			form.funding = angular.isDefined($attrs.funding);

			form.id = $attrs.volume || undefined;
			form.access = page.$parse($attrs.access)($scope) || undefined;

			form.data = {
				access: form.access.access || 0,
				inherit: form.access.inherit || 0,
			};

			if (form.access.funding) {
				form.data.funding = form.access.funding;
			}
			//

			form.extend = function () {
				form.data.inherit = form.data.access === form.data.inherit ? 0 : Math.min(form.data.access, page.constants.data.permissionName.CONTRIBUTE);

				form.save();
			};

			//

			form.saveFn = undefined;
			form.successFn = undefined;
			form.errorFn = undefined;

			form.save = function () {
				form.volumeAccess = new page.models.VolumeAccess(form.data);

				if (form.data.inherit > form.data.access)
					form.data.inherit = form.data.access;

				if (angular.isFunction(form.saveFn)) {
					form.saveFn(form);
				}

				form.volumeAccess.$save({
					id: form.id,
					partyId: form.access.party.id,
				}, function () {
					if (angular.isFunction(form.successFn)) {
						form.successFn(form, arguments);
					}

					form.$setPristine();
				}, function (res) {
					form.messages.addError({
						body: page.constants.message('access.grant.save.error'),
						report: res,
					});

					if (angular.isFunction(form.errorFn)) {
						form.errorFn(form, arguments);
					}
				});
			};

			//

			form.removeFn = undefined;
			form.removeSuccessFn = undefined;
			form.removeErrorFn = undefined;

			form.remove = function () {
				if ((form.funding && !form.data.access) || (!form.funding && !form.data.funding)) {
					form.volumeAccess = new page.models.VolumeAccess();

					if (angular.isFunction(form.removeFn)) {
						form.removeFn(form);
					}

					form.volumeAccess.$delete({
						id: form.id,
						partyId: form.access.party.id,
					}, function () {
						if (angular.isFunction(form.removeSuccessFn)) {
							form.removeSuccessFn(form, arguments, form.access);
						}

						form.$setPristine();
					}, function (res) {
						form.messages.addError({
							body: page.constants.message('access.grant.remove.error'),
							report: res,
						});

						if (angular.isFunction(form.removeErrorFn)) {
							form.removeErrorFn(form, arguments, form.access);
						}
					});
				} else {
					if (form.funding) {
						delete form.data.funding;
					} else {
						delete form.data.access;
						delete form.data.inherit;
					}

					form.volumeAccess = new page.models.VolumeAccess(form.data);

					if (angular.isFunction(form.removeFn)) {
						form.removeFn(form);
					}

					form.volumeAccess.$save({
						id: form.id,
						partyId: form.access.party.id,
					}, function () {
						if (angular.isFunction(form.successFn)) {
							form.removeSuccessFn(form, arguments, form.access);
						}

						form.$setPristine();
					}, function (res) {
						form.messages.addError({
							body: page.constants.message('access.grant.save.error'),
							report: res,
						});

						if (angular.isFunction(form.errorFn)) {
							form.removeErrorFn(form, arguments, form.access);
						}
					});
				}
			};

			//

			$scope.$emit('accessGrantForm-init', form, $scope);
		};

		//

		return {
			restrict: 'E',
			templateUrl: 'accessGrantForm.html',
			scope: false,
			replace: true,
			link: link
		};
	}
]);
