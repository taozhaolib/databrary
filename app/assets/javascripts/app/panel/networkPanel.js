define(['app/config/module'], function (module) {
	'use strict';

	module.controller('NetworkPanel', ['$scope', 'Party', '$routeParams', '$filter', 'PartyAuthorize', 'ConstantService', function ($scope, Party, $routeParams, $filter, PartyAuthorize, constantService) {
		$scope.constant = $scope.constant || constantService;

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

		//

		$scope.partyAuth = [];

		$scope.onModeAuthorize = function () {
			PartyAuthorize.query(function (data) {
				$scope.partyAuth = data;
			});
		};

		//

		$scope.currentAuthChild = undefined;
		$scope.currentAuthForm = undefined;

		$scope.openAuthChild = function (child, form) {
			if(!$scope.currentAuthForm || !$scope.currentAuthForm.$dirty) {
				$scope.currentAuthChild = child;
				$scope.currentAuthForm = form;
			}
		};

		$scope.closeAuthChild = function () {
			$scope.currentAuthChild = undefined;
			$scope.currentAuthForm = undefined;
		};

		//

		$scope.authChildShortcut = undefined;

		$scope.shortcutAuthChild = function (key) {
			$scope.authChildShortcut = key;
		};
	}]);
});
