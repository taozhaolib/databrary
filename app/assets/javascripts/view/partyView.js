module.controller('PartyView', [
	'$scope', 'party', 'volumes', 'pageService', function ($scope, party, volumes, page) {
		$scope.party = party;
		$scope.volumes = volumes;

		page.display.title = party.name;

		$scope.browser.initialize('party', volumes);

		$scope.$watchCollection('party', function () {
			page.events.talk('panelService-refresh');
		});

		$scope.$watchCollection('volumes', function () {
			page.events.talk('panelService-refresh');
		});
	}
]);
