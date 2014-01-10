define(['app/config/module'], function (module) {
	'use strict';

	module.controller('NetworkPanel', ['$scope', 'Party', '$routeParams', '$filter', function ($scope, Party, $routeParams, $filter) {
		$scope.bootPanel = function () {
//			if (!$scope.party || !$scope.party.parents || !$scope.party.children)
//				Party.get({
//					id: $routeParams.id,
//					parents: '',
//					children: ''
//				}, function (party) {
//					$scope.party = party;
//					$scope.updateNetwork();
//				});
		};

		//

		$scope.updateNetwork = function () {
			$scope.network = [];
			$scope.parents = [];
			$scope.children = [];

			angular.forEach($scope.party.parents, function (partyAuth) {
				$scope.parents.push(partyAuth.party);
			});

			angular.forEach($scope.party.children, function (partyAuth) {
				$scope.children.push(partyAuth.party);
			});
		};

		$scope.$watch('party', function () {$scope.updateNetwork();}, true);
	}]);
});
