module.directive('partyEditGrantForm', [
	'pageService', function (page) {
		var link = function ($scope) {
			var form = $scope.partyEditGrantForm;

			form.data = {};

			//

			form.init = function (party) {
				form.party = form.party || party;
				form.data = page.$filter('toArray')(form.party.children);
			};

			//

			page.events.talk('partyEditGrantForm-init', form, $scope);
		};

		//

		return {
			restrict: 'E',
			templateUrl: 'partyEditGrantForm.html',
			scope: false,
			replace: true,
			link: link
		};
	}
]);
