define(['config/module'], function (module) {
	'use strict';

	module.controller('NetworkPanel', ['$scope', '$routeParams', '$filter', 'PartyAuthorize', 'ConstantService', 'EventService', '$cacheFactory', 'Page', function ($scope, $routeParams, $filter, PartyAuthorize, constantService, eventService, $cacheFactory, page) {
		$scope.constant = $scope.constant || constantService;

		var $httpCache = $cacheFactory.get('$http');

		$scope.bootPanel = function () {
			getPartyAuth();
		};

		$scope.refreshPanel = function () {
			getPartyAuth();
		};

		//

		$scope.partyAuth = [];

		var getPartyAuth = function () {
			$httpCache.removeAll();

			PartyAuthorize.query(function (data) {
				$scope.partyAuth = data;

				$scope.parents = [];
				$scope.children = [];

				angular.forEach(data.parents, function (partyAuth) {
					$scope.parents.push(partyAuth.party);
				});

				angular.forEach(data.children, function (partyAuth) {
					$scope.children.push(partyAuth.party);
				});
			}, function (res) {
				page.messages.addError({
					body: page.constants.message('network.authquery.error'),
					errors: res[0],
					status: res[1]
				})
			});
		};

		//

		$scope.resetAuth = {};

		$scope.currentAuthParent = undefined;
		$scope.currentAuthChild = undefined;

		var childWatch = undefined;

		//

		$scope.openAuthChild = function (child) {
			$scope.resetAuthChild(child);
			$scope.currentAuthChild = child;
		};

		$scope.resetAuthChild = function (child) {
			if (angular.isDefined($scope.currentAuthChild) && angular.isDefined($scope.resetAuth))
				angular.extend($scope.currentAuthChild, $scope.resetAuth);

			if (angular.isFunction(childWatch))
				childWatch();

			if (!angular.isDefined(child))
				return;

			$scope.resetAuth = {
				direct: child.direct,
				inherit: child.inherit,
				expires: child.expires,
				id: child.id
			};

			if (!$scope.auth.hasAccess('ADMIN', $scope.party))
				childWatch = $scope.$watch(function () {
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
		};

		//

		var grantCancelFn = function () {
			getPartyAuth();
			$scope.resetAuthChild();

			if ($scope.currentAuthChild.force)
				delete $scope.partyAuth.children[$scope.currentAuthChild.party.id];

			$scope.currentAuthChild = undefined;
		};

		eventService.listen($scope, 'authGrantForm-init', function (event, form, $scope) {
			form.other = $scope.child;
			form.successFn = grantCancelFn;
			form.cancelFn = grantCancelFn;
			form.denySuccessFn = grantCancelFn;
			event.stopPropagation();
		});

		//

		$scope.openAuthParent = function (parent) {
			if (!parent.force)
				return;

			$scope.currentAuthParent = parent;
		};

		//

		var applySuccessFn = function () {
			delete $scope.currentAuthParent['force'];
			applyCancelFn();
		};

		var applyCancelFn = function () {
			if ($scope.currentAuthParent.force)
				delete $scope.partyAuth.parents[$scope.currentAuthParent.party.id];

			$scope.currentAuthParent = undefined;
		};

		eventService.listen($scope, 'authApplyForm-init', function (event, form, $scope) {
			form.other = $scope.parent;
			form.successFn = applySuccessFn;
			form.cancelFn = applyCancelFn;
			event.stopPropagation();
		});

		//

		var selectFn = function (found, form) {
			var request = {
				party: found,
				force: true,
				id: found.id,
				inherit: 0,
				direct: 0
			};

			if (form.apply) {
				$scope.partyAuth.children[found.id] = request;
				$scope.openAuthChild(request);
				$scope.currentAuthChild = request;
			} else {
				$scope.partyAuth.parents[found.id] = request;
				$scope.currentAuthParent = request;
			}
		};

		eventService.listen($scope, 'authSearchForm-init', function (event, form) {
			form.selectFn = selectFn;
			event.stopPropagation();
		});
	}]);
});
