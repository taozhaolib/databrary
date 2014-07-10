'use strict';

module.controller('NetworkPanel', [
	'$scope', 'pageService', function ($scope, page) {
		$scope.bootPanel = function () {
			getPartyAuth();
		};

		$scope.refreshPanel = function () {
			getPartyAuth();
		};

		//

		$scope.partyAuth = {
			parents: {},
			children: {},
		};

		var actionMessages = {};

		$scope.$on('$destroy', function () {
			angular.forEach(actionMessages, function (bundle) {
				page.messages.remove(bundle.message);
			});
		});

		var getPartyAuth = function () {
			if (page.auth.hasAccess('ADMIN', $scope.party)) {
				page.models.PartyAuthorize.$cache.removeAll();
				page.models.PartyAuthorize.query(function (data) {
					$scope.partyAuth = data;

					angular.forEach($scope.partyAuth.children, function (party) {
						if (!party.member && !party.site) {
							if (!actionMessages[party.id]) {
								actionMessages[party.id] = {
									party: party,
									message: page.messages.add({
										type: 'yellow',
										closeable: true,
										body: page.$compile('<span>' + page.constants.message('auth.pending.notice', party.party.name) + ' <a href="' + page.router.partyEdit($scope.party, 'grant') + '">Manage</a>.</span>')($scope)
									})
								};
							}
							else {
								actionMessages[party.id].party = party;
							}
						}
					});
				}, function (res) {
					page.messages.addError({
						body: page.constants.message('network.authquery.error'),
						report: res,
					});
				});
			} else {
				page.models.Party.$cache.removeAll();
				page.models.Party.get({
					id: page.$routeParams.id || page.auth.user.id,
					parents: '',
					children: ''
				}, function (data) {
					$scope.partyAuth = {
						parents: {},
						children: {}
					};

					angular.forEach(data.parents, function (party) {
						$scope.partyAuth.parents[party.id] = {
							id: party.id,
							party: party
						};
					});

					angular.forEach(data.children, function (party) {
						$scope.partyAuth.children[party.id] = {
							id: party.id,
							party: party
						};
					});
				}, function (res) {
					page.messages.addError({
						body: page.constants.message('network.authquery.error'),
						report: res,
					});
				});
			}
		};

		//

		var grantCancelFn = function () {
			getPartyAuth();
			$scope.resetAuthChild();

			if ($scope.currentAuthParent && $scope.currentAuthParent.remote) {
				if ($scope.currentAuthParent.force) {
					delete $scope.partyAuth.parents[$scope.currentAuthParent.party.id];
				}

				$scope.currentAuthParent = undefined;
			} else {
				if ($scope.currentAuthChild.force) {
					delete $scope.partyAuth.children[$scope.currentAuthChild.party.id];
				}

				$scope.currentAuthChild = undefined;
			}
		};

		page.events.listen($scope, 'authGrantForm-init', function (event, form, $scope) {
			if ($scope.currentAuthParent && $scope.currentAuthParent.remote) {
				form.party = page.auth.user;
			}

			form.other = $scope.currentAuthParent && $scope.currentAuthParent.remote ? {
				party: $scope.party,
				id: $scope.party.id,
				inherit: page.permission.DOWNLOAD,
				direct: page.permission.DOWNLOAD,
			} : $scope.currentAuthChild;

			form.successFn = grantCancelFn;
			form.cancelFn = grantCancelFn;
			form.denySuccessFn = grantCancelFn;
			event.stopPropagation();
		});

		//

		var applySuccessFn = function () {
			if ($scope.currentAuthChild && $scope.currentAuthChild.remote) {
				delete $scope.currentAuthChild['force'];
			}
			else {
				delete $scope.currentAuthParent['force'];
			}

			applyCancelFn();
		};

		var applyCancelFn = function () {
			getPartyAuth();

			if ($scope.currentAuthChild && $scope.currentAuthChild.remote) {
				if ($scope.currentAuthChild.force) {
					delete $scope.partyAuth.children[$scope.currentAuthChild.party.id];
				}

				$scope.currentAuthChild = undefined;
			} else {
				if ($scope.currentAuthParent.force) {
					delete $scope.partyAuth.parents[$scope.currentAuthParent.party.id];
				}

				$scope.currentAuthParent = undefined;
			}
		};

		page.events.listen($scope, 'authApplyForm-init', function (event, form, $scope) {
			if ($scope.currentAuthChild && $scope.currentAuthChild.remote) {
				form.party = page.auth.user;
			}

			form.notFound.query = $scope.currentAuthChild && $scope.currentAuthChild.remote ? $scope.currentAuthChild.query : $scope.currentAuthParent.query;

			form.other = $scope.currentAuthChild && $scope.currentAuthChild.remote ? {
				party: $scope.party,
				id: $scope.party.id,
				inherit: page.permission.DOWNLOAD,
				direct: page.permission.DOWNLOAD
			} : $scope.currentAuthParent;

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
				inherit: page.permission.DOWNLOAD,
				direct: page.permission.DOWNLOAD
			};

			if (form.apply) {
				$scope.partyAuth.children[found.id] = request;
				$scope.openAuthChild(request);
			} else {
				$scope.partyAuth.parents[found.id] = request;
				$scope.openAuthParent(request);
			}
		};

		var notFoundFn = function (query, form) {
			var request = {
				party: {
					id: -1,
					name: page.constants.message('auth.request.notfound.user'),
					avatar: '/party/-1/avatar'
				},
				force: true,
				id: -1,
				query: query
			};

			if (form.apply) {
				page.messages.add({
					type: 'yellow',
					countdown: 3000,
					body: page.constants.message('auth.grant.notfound.message')
				});
			} else {
				$scope.partyAuth.parents[-1] = request;
				$scope.openAuthParent(request);
			}
		};

		page.events.listen($scope, 'authSearchForm-init', function (event, form) {
			form.selectFn = selectFn;
			form.notFoundFn = notFoundFn;
			event.stopPropagation();
		});

		//

		var isAdmin = function () {
			return page.auth.hasAccess('ADMIN', $scope.party);
		};

		$scope.isForeign = function () {
			return $scope.party.id != page.auth.user.id;
		};

		$scope.showExtended = function () {
			return isAdmin()
		};

		//

		$scope.presetName = function (type, name, party) {
			if (angular.isString(party)) {
				return '<strong>' + page.constants.message('auth.' + type + '.' + name + '.title') + '</strong>: ' + page.constants.message('auth.' + type + '.' + name, party);
			} else {
				return '<strong>' + page.constants.message('auth.' + type + '.' + name + '.title') + '</strong>: ' + page.$filter('possessive')('auth.' + type + '.' + name, party);
			}
		};

		//

		$scope.canGrant = function () {
			if (!$scope.isForeign()) {
				return false;
			}

			var exists;

			angular.forEach($scope.partyAuth.parents, function (parent) {
				if (parent.party.id == page.auth.user.id) {
					exists = true;
					return false;
				}
			});

			return !exists;
		};

		$scope.canApply = function () {
			if (!$scope.isForeign()) {
				return false;
			}

			var exists;

			angular.forEach($scope.partyAuth.children, function (child) {
				if (child.party.id == page.auth.user.id) {
					exists = true;
					return false;
				}
			});

			return !exists;
		};

		$scope.grant = function () {
			page.$location.url(page.router.partyEdit(page.auth.user, 'grant'));
			var remove = page.events.listen(page.$rootScope, 'partyEditGrantForm-init', function (event, form, $thatScope) {
				remove();

				remove = page.events.listen($thatScope, 'authSearchForm-init', function (event, searchForm) {
					if (searchForm.principal != 'child') {
						return;
					}

					searchForm.selectFn($scope.party);
					form.scrollToFuture($scope.party);
					remove();
				});
			});
		};

		$scope.apply = function () {
			page.$location.url(page.router.partyEdit(page.auth.user, 'apply'));
			var remove = page.events.listen(page.$rootScope, 'partyEditApplyForm-init', function (event, form, $thatScope) {
				remove();

				remove = page.events.listen($thatScope, 'authSearchForm-init', function (event, searchForm) {
					if (searchForm.principal == 'child') {
						return;
					}

					searchForm.selectFn($scope.party);
					form.scrollToFuture($scope.party);
					remove();
				});
			});
		};
	}
]);
