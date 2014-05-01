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
			}
		};

		$scope.save = function () {
			var party = new page.models.Party(form.data);

			party.$save({
				id: $scope.party.id
			}, function (res) {
				console.log(res);

				$scope.cancel();
			}, function (res) {
				console.log(res);
			});
		};

		$scope.cancel = function () {
			$scope.editMode = false;
			form.data = {};
		};
	}
]);
