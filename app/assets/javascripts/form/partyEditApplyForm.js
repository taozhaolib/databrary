module.directive('partyEditApplyForm', [
	'pageService', function (page) {
		var link = function ($scope) {
			var form = $scope.partyEditApplyForm;

			form.data = {};

			//

			form.init = function (party, parents) {
				form.party = form.party || party;
				form.data = page.$filter('toArray')(parents);
			};

			//

			//

			page.events.talk('partyEditApplyForm-init', form, $scope);
		};

		//

		return {
			restrict: 'E',
			templateUrl: 'partyEditApplyForm.html',
			scope: false,
			replace: true,
			link: link
		};
	}
]);
