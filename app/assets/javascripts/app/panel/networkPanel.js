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
		$scope.currentAuthFormWatch = undefined;

		$scope.openAuthChild = function (child, form) {
			$scope.resetAuthChild(child);

			$scope.currentAuthChild = child;
			$scope.currentAuthForm = form;

			//

			var custom = undefined;

			child.preset = undefined;

			angular.forEach($scope.constant.data.preset, function (preset) {
				if (child.direct == preset.direct && child.inherit == preset.inherit)
					$scope.setPreset(child, preset);

				if (angular.isUndefined(preset.inherit))
					custom = preset;
			});

			if (angular.isUndefined(child.preset))
				$scope.setPreset(child, custom);

			//

			child.expiration = $filter('date')(new Date(child.authorized), 'yyyy-MM-dd');
		};

		$scope.closeAuthChild = function () {
			$scope.resetAuthChild();

			$scope.currentAuthChild = undefined;
			$scope.currentAuthForm = undefined;
		};

		$scope.resetAuthChild = function (child) {
			if (angular.isDefined($scope.currentAuthChild) && angular.isDefined($scope.resetAuth))
				angular.extend($scope.currentAuthChild, $scope.resetAuth);

			if (angular.isFunction($scope.currentAuthFormWatch))
				$scope.currentAuthFormWatch();

			if (angular.isDefined(child)) {
				$scope.resetAuth = {
					direct: child.direct,
					inherit: child.inherit,
					authorized: child.authorized,
					id: child.id
				};

				if (!$scope.auth.hasAccess('ADMIN', $scope.party))
					$scope.currentAuthFormWatch = $scope.$watch(function () {
						return [child.direct, child.inherit];
					}, function (newVal, oldVal) {
						if (newVal[0] != oldVal[0]) {
							switch (child.inherit) {
								case 4:
									if (child.direct == 3)
										child.inherit = 3;
									else if (child.direct != 4)
										child.inherit = 2;
									break;

								case 3:
									if (child.direct < 3)
										child.inherit = 2;
									break;
							}
						} else if (newVal[1] != oldVal[1]) {
							switch (child.direct) {
								case 3:
									if (child.inherit == 4)
										child.direct = 4;
									break;

								default:
									if (child.inherit >= 3)
										child.direct = child.inherit;
									break;
							}
						}
					}, true);
			}
		};

		//

		$scope.setPreset = function (child, preset) {
			child.preset = preset;

			if (preset && angular.isDefined(preset.inherit)) {
				child.inherit = preset.inherit;
				child.direct = preset.direct;
			}
		};

		$scope.presetSelected = function (child, preset) {
			if (child.preset == preset)
				return 'checked';

			return '';
		};
	}]);
});
