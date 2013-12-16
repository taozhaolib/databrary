define(['app/config/module'], function (module) {
	'use strict';

	module.controller('PartyView', ['$scope', 'party', function ($scope, party) {
		$scope.view = {
			view: 'party',
			party: party
		};
		$scope.party = party;
	}]);
});
