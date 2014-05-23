module.controller('PartyView', [
	'$scope', 'party', 'volumes', 'pageService', function ($scope, party, volumes, page) {
		$scope.party = party;
		$scope.volumes = volumes;

		page.display.title = party.name;
		page.display.toolbarLinks = [
			{
				type: 'yellow',
				html: page.constants.message('party.edit'),
				url: page.router.partyEdit(party),
				access: 'CONTRIBUTE',
				object: party,
			},
			{
				type: 'green',
				html: page.constants.message('volume.edit.create'),
				url: page.router.volumeCreate(),
				access: 'ADMIN',
				object: party,
			},
		];

		$scope.browser.initialize('party', volumes);

		$scope.$watchCollection('party', function () {
			page.events.talk('panelService-refresh');
		});

		$scope.$watchCollection('volumes', function () {
			page.events.talk('panelService-refresh');
		});
	}
]);
