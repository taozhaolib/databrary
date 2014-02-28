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

		$scope.resetAuth = {};

		$scope.currentAuthChild = undefined;
		$scope.currentAuthForm = undefined;

		$scope.openAuthChild = function (child, form) {
			if ($scope.currentAuthForm && $scope.currentAuthForm.$dirty)
				$scope.resetAuthChild(child);

			$scope.currentAuthChild = child;
			$scope.currentAuthForm = form;
		};

		$scope.closeAuthChild = function () {
			$scope.resetAuthChild();

			$scope.currentAuthChild = undefined;
			$scope.currentAuthForm = undefined;
		};

		$scope.resetAuthChild = function (child) {
			angular.extend($scope.currentAuthChild, $scope.resetAuth);

			if (angular.isDefined(child)) {
				$scope.resetAuth = {
					direct: child.direct,
					inherit: child.inherit,
					authorized: child.authorized,
					id: child.id
				};
			}
		};

		//

		$scope.setPreset = function (child, preset) {
			child.currentPreset = preset;

			child.inherit = preset.inherit;
			child.direct = preset.direct;
		};

		$scope.presetSelected = function (child, preset) {
			if(child.preset = preset)
				return 'checked';
		};

		$scope.initializePreset = function (child, presets) {

		};
	}]);
});
