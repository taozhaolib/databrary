define(['app/config/module'], function (module) {
	'use strict';

	module.controller('NetworkPanel', ['$scope', 'Party', '$routeParams', '$filter', function ($scope, Party, $routeParams, $filter) {
		$scope.bootPanel = function () {
			$scope.updateNetwork();
		};

		$scope.refreshPanel = function () {
			$scope.updateNetwork();
			$scope.enabled = $scope.parents.length > 0 || $scope.children.length > 0;
		};

		//

		$scope.updateNetwork = function () {
			$scope.parents = [];
			$scope.children = [];

			angular.forEach($scope.party.parents, function (partyAuth) {
				$scope.parents.push(partyAuth.party);
			});

			angular.forEach($scope.party.children, function (partyAuth) {
				$scope.children.push(partyAuth.party);
			});
		};
	}]);
});
