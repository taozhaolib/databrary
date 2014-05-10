module.controller('OverviewPartyPanel', [
	'$scope', 'pageService', function ($scope, page) {
		$scope.editMode = false;

		var form;

		$scope.edit = function () {
			$scope.editMode = true;
			form = $scope.userEditForm;

			form.data = {
				name: $scope.party.name,
				email: $scope.party.email,
				orcid: $scope.party.orcid,
				duns: $scope.party.duns,
				affiliation: $scope.party.affiliation,
				openid: $scope.party.openid
			};

			if (page.auth.hasAuth('ADMIN', $scope.party)) {
				form.data.auth = '';
				form.data.password = {
					once: '',
					again: ''
				};
			}
		};

		$scope.save = function () {
			var party = new page.models.Party(form.data);

			party.$save({
				id: $scope.party.id
			}, function (res) {
				page.messages.add({
					body: page.constants.message('party.edit.success'),
					type: 'green',
					countdown: 3000
				});

				page.models.Party.$cache.removeAll();
				page.$route.reload();
			}, function (res) {
				page.messages.addError({
					body: page.constants.message('party.edit.error'),
					report: res
				});
			});
		};

		$scope.cancel = function () {
			$scope.editMode = false;
			form.data = {};
		};
	}
]);
