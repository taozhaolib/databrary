module.directive('accessGrantForm', [
	'pageService', function (page) {
		var link = function ($scope, $element, $attrs) {
			var form = $scope.accessGrantForm;
			form.id = $attrs.volume || undefined;
			form.access = page.$parse($attrs.access)($scope) || undefined;

			form.data = {
				individual: form.access.individual || 0,
				children: form.access.children || 0,
			};

			form.data.extend = form.data.children !== 0;

			var backup = $.extend(true, {}, form.data);

			//

			form.saveFn = undefined;
			form.successFn = undefined;
			form.errorFn = undefined;

			form.save = function () {
				form.data.children = form.data.extend ? form.data.individual : 0;

				form.volumeAccess = new page.models.VolumeAccess(form.data);

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

					form.messages.add({
						body: page.constants.message('access.grant.save.success'),
						type: 'green',
						countdown: 3000,
					});

					backup = $.extend(true, {}, form.data);
					form.$setPristine();
					page.models.Volume.$cache.removeAll();
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

			form.resetFn = undefined;

			form.reset = function () {
				if (angular.isFunction(form.resetFn)) {
					form.resetFn(form);
				}

				form.validator.clearServer();

				form.data = $.extend(true, {}, backup);

				if (form.access.new) {
					form.remove();
				} else {
					form.$setPristine();
				}
			};

			//

			form.removeFn = undefined;
			form.removeSuccessFn = undefined;
			form.removeErrorFn = undefined;

			form.remove = function () {
				if (angular.isFunction(form.removeFn)) {
					form.removeFn(form);
				}

				page.models.VolumeAccess.delete({
					id: form.id,
					partyId: form.access.party.id,
				}, {}, function () {
					if (angular.isFunction(form.removeSuccessFn)) {
						form.removeSuccessFn(form, arguments, form.access);
					}

					form.messages.add({
						body: page.constants.message('access.grant.remove.success'),
						type: 'green',
						countdown: 3000,
					});

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
