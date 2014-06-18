module.directive('partyEditAccountForm', [
	'pageService', function (page) {
		var link = function ($scope) {
			var form = $scope.partyEditAccountForm;

			form.data = {};
			form.authors = [];
			var backup = {};

			form.saveFn = undefined;
			form.resetFn = undefined;
			form.successFn = undefined;
			form.errorFn = undefined;

			//

			form.init = function (party) {
				form.party = form.party || party;
				form.data = {
					email: party.email,
				};

				backup = $.extend(true, {}, form.data);
			};

			//

			form.save = function () {
				if (angular.isFunction(form.saveFn)) {
					form.saveFn(form);
				}

				page.models.Party.save({
						id: form.party.id,
					}, form.data,
					function (res) {
						form.validator.server({});

						form.messages.add({
							type: 'green',
							countdown: 3000,
							body: page.constants.message('party.edit.profile.success'),
						});

						backup = $.extend(true, {}, form.data);

						if (angular.isFunction(form.successFn)) {
							form.successFn(form, res);
						}

						form.$setPristine();
						page.models.Party.$cache.removeAll();
					}, function (res) {
						form.validator.server(res);

						if (angular.isFunction(form.errorFn)) {
							form.errorFn(form, res);
						}
					});
			};

			form.reset = function () {
				if (angular.isFunction(form.resetFn))
					form.resetFn(form);

				form.data = $.extend(true, {}, backup);
				form.$setPristine();
			};

			//

			form.validator.client({
				email: {
					tips: page.constants.message('party.edit.email.help')
				},
				password: {
					tips: page.constants.message('party.edit.password.help')
				},
				'password.again': {
					tips: page.constants.message('party.edit.password.again.help')
				},
				auth: {
					tips: page.constants.message('party.edit.auth.help')
				},
			}, true);

			//

			page.events.talk('partyEditAccountForm-init', form, $scope);
		};

		//

		return {
			restrict: 'E',
			templateUrl: 'partyEditAccountForm.html',
			scope: false,
			replace: true,
			link: link
		};
	}
]);
