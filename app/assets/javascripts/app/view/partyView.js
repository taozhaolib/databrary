define(['app/config/module'], function (module) {
	'use strict';

	module.controller('PartyView', ['$scope', 'party', function ($scope, party) {
		$scope.party = party;

		//

		var start = function () {
			$scope.id = 0;
		};

		start();
	}]);
});
