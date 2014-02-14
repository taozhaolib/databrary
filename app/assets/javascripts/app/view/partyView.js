define(['app/config/module'], function (module) {
	'use strict';

	module.controller('PartyView', ['$scope', 'party', 'volumes', 'PanelService', function ($scope, party, volumes, panelService) {
		$scope.party = party;
		$scope.volumes = volumes;

		$scope.browser.initialize('party', volumes);

		$scope.$watch('party', function () {
			panelService.refreshPanels();
		}, true);
	}]);
});
