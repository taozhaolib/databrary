define(['config/module'], function (module) {
	'use strict';

	module.controller('PartyView', ['$scope', 'party', 'PanelService', 'PageService', function ($scope, party, panelService, page) {
		$scope.party = party;
		page.title = party.name;

		var volumes = [];

		$scope.browser.initialize('party', party.volumes);

		$scope.$watch('party', function () {
			panelService.refreshPanels();
		}, true);
	}]);
});
