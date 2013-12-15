define(['app/config/module'], function (module) {
	'use strict';

	module.controller('OverviewPartyPanel', ['$scope', 'Party', '$routeParams', function ($scope, Party, $routeParams) {
		$scope.bootPanel = function () {
			if (!$scope.party)
				Party.get({
					id: $routeParams.id,
					funding: 'all'
				}, function (party) {
					$scope.party = party;
				});
		};
	}]);
});
