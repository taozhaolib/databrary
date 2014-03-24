define(['config/module'], function (module) {
	'use strict';

	module.controller('PartyView', ['$scope', 'party', 'PanelService', function ($scope, party, panelService) {
		$scope.party = party;

		var volumes = [];

		$scope.browser.initialize('party', party.volumes);

		$scope.$watch('party', function () {
			panelService.refreshPanels();
		}, true);
	}]);
});
