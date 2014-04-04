define(['config/module'], function (module) {
	'use strict';

	module.controller('PartyView', ['$scope', 'party', 'volumes', 'panelService', 'pageService', function ($scope, party, volumes, panelService, page) {
		$scope.party = party;
		$scope.volumes = volumes;

		page.title = party.name;

		$scope.browser.initialize('party', volumes);

		$scope.$watch('party', function () {
			panelService.refreshPanels();
		}, true);
	}]);
});
