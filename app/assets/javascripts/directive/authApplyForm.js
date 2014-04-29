module.directive('authApplyForm', [
	'PartyAuthorize',
	'authService',
	'authPresetService',
	'pageService',
	function (PartyAuthorize, authService, authPresetService, page) {
		var link = function ($scope) {
			var form = $scope.authApplyForm;

			$scope.page = page;
			$scope.constant = page.constants;

			form.presets = authPresetService;
			form.party = $scope.party || authService.user;
			form.other = undefined;
			form.notFound = {
				query: undefined,
				info: undefined
			};

			//

			form.saveFn = undefined;
			form.successFn = undefined;
			form.errorFn = undefined;

			var saveAuth = function () {
				form.partyAuthorize = new PartyAuthorize();

				form.partyAuthorize.direct = form.other.direct;
				form.partyAuthorize.inherit = form.other.inherit;

				form.partyAuthorize.$apply({
					id: form.party.id,
					partyId: form.other.party.id
				}, function () {
					if (angular.isFunction(form.successFn))
						form.successFn(form, arguments);
				}, function (res) {
					page.messages.addError({
						body: page.constants.message('auth.apply.error'),
						errors: res[0],
						status: res[1]
					});

					if (angular.isFunction(form.errorFn))
						form.errorFn(form, arguments);
				});
			};

			var saveQuery = function () {
				form.partyAuthorize = new PartyAuthorize();

				form.partyAuthorize.$search({
					id: form.party.id,
					apply: true,
					notfound: true,
					name: form.notFound.query,
					target: form.notFound.info
				}, function (res) {
					page.messages.add({
						type: 'green',
						countdown: 3000,
						body: page.constants.message('auth.request.notfound.success')
					});

					if (angular.isFunction(form.successFn))
						form.successFn(form, arguments);
				}, function (res) {
					page.messages.addError({
						body: page.constants.message('error.generic'),
						errors: res[0],
						status: res[1]
					});

					if (angular.isFunction(form.errorFn))
						form.errorFn(form, arguments);
				});
			};

			form.save = function () {
				if (angular.isFunction(form.saveFn))
					form.saveFn(form);

				if (form.notFound.query)
					saveQuery();
				else
					saveAuth();
			};

			//

			form.cancelFn = undefined;

			form.cancel = function () {
				if (angular.isFunction(form.cancelFn))
					form.cancelFn(form);

				if (form.other) {
					form.other.inherit = 0;
					form.other.direct = 0;
					form.other.preset = undefined;
				}
			};

			//

			page.events.talk('authApplyForm-init', form, $scope);
		};

		//

		return {
			restrict: 'E',
			templateUrl: 'authApplyForm.html',
			scope: false,
			replace: true,
			link: link
		};
	}
]);
