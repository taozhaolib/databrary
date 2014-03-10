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
				$scope.parents.push(partyAuth);
			});

			angular.forEach($scope.party.children, function (partyAuth) {
				$scope.children.push(partyAuth);
			});
		};

		//

		$scope.partyAuth = [];
		$scope.parental = undefined;

		var getPartyAuth = function () {
			PartyAuthorize.query(function (data) {
				$scope.partyAuth = data;
			});
		};

		$scope.onModeAuthorize = function () {
			getPartyAuth();
			$scope.parental = true;
		};

		$scope.onModeApply = function () {
			getPartyAuth();
			$scope.parental = false;
		};

		//

		$scope.resetAuth = {};

		$scope.currentAuthParent = undefined;
		$scope.currentAuthChild = undefined;

		$scope.currentAuthForm = undefined;
		$scope.currentAuthFormWatch = undefined;

		//

		$scope.openAuthParent = function (parent, form) {
			if (parent.authorized)
				return;

//			$scope.resetAuthParent(parent);

			$scope.currentAuthParent = parent;
			$scope.currentAuthForm = form;
		};

		$scope.closeAuthParent = function () {
//			$scope.resetAuthParent();

			$scope.currentAuthChild = undefined;
			$scope.currentAuthForm = undefined;
		};

		//

		$scope.openAuthChild = function (child, form) {
			$scope.resetAuthChild(child);

			$scope.currentAuthChild = child;
			$scope.currentAuthForm = form;

			//

			var custom = undefined,
				presets = $scope.getPresets(child);

			child.preset = undefined;

			angular.forEach(presets, function (preset) {
				if (child.direct == preset.direct && child.inherit == preset.inherit)
					$scope.setPreset(child, preset);

				if (angular.isUndefined(preset.inherit))
					custom = preset;
			});

			if (angular.isUndefined(child.preset))
				$scope.setPreset(child, custom);

			//

			if (child.expires)
				child.expiration = $filter('date')(new Date(child.expires), 'yyyy-MM-dd');
			else
				child.expiration = '';
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
					expires: child.expires,
					id: child.id
				};

				if (!$scope.auth.hasAccess('ADMIN', $scope.party))
					$scope.currentAuthFormWatch = $scope.$watch(function () {
						return [child.direct, child.inherit, child.expiration];
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

				$scope.currentAuthExpirationWatch = $scope.$watch(function () {
					return child.expiration;
				}, function (newVal, oldVal) {
					var now = new Date(),
						limit = new Date(now.setYear(now.getFullYear() + 2)).getTime(),
						exp = child.expiration.split('-'),
						trial = new Date(exp[1] + '-' + exp[2] + '-' + exp[0]).getTime();

					if (trial > limit)
						child.expiration = $filter('date')(limit, 'yyyy-MM-dd');
				});
			}
		};

		$scope.saveAuthChild = function () {
			if (angular.isUndefined($scope.currentAuthChild))
				return false;

			var authChild = new PartyAuthorize();

			var exp = $scope.currentAuthChild.expiration.split('-');

			authChild.direct = $scope.currentAuthChild.direct;
			authChild.inherit = $scope.currentAuthChild.inherit;
			authChild.expires = $scope.currentAuthChild.expiration;

			if (!authChild.expires.match(/^\d{4}-\d{1,2}-\d{1,2}$/))
				authChild.expires = '';

			var newVals = [authChild.direct, authChild.inherit, authChild.expires];

			authChild.$save({
				id: $scope.party.id,
				partyId: $scope.currentAuthChild.id
			}, function (data) {
				var current = $scope.currentAuthChild;

				$scope.closeAuthChild();

				current.direct = newVals[0];
				current.inherit = newVals[1];
				current.expires = newVals[2];
			}, function () {
				console.log(arguments);
			});

			return true;
		};

		//

		$scope.getPresets = function (other) {
			if ($scope.parental === false) {
				if (other.institution || other.id == 0)
					return $scope.constant.data.preset.institution.slice(0, 2);
				else
					return $scope.constant.data.preset.individual.slice(0, 3);
			} else {
				if ($scope.party.institution || $scope.party.id == 0)
					return $scope.constant.data.preset.institution;
				else
					return $scope.constant.data.preset.individual;
			}
		};

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

		//

		$scope.searchParties = function (form) {
			if (!form.name)
				return form.found = [];

			PartyAuthorize.search({
				apply: !$scope.parental,
				name: form.name
			}, function (data) {
				form.found = data;
			});
		};

		$scope.selectFound = function (found) {
			var request = {
				party: found,
				force: true,
				inherit: 0,
				direct: 0
			};

			$scope.partyAuth.parents[found.id] = request;
		};
	}]);
});
