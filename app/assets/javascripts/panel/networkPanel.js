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
			children: {}
		};

		var actionMessages = {};

		var getPartyAuth = function () {
			if (page.auth.hasAccess('ADMIN', $scope.party)) {
				page.models.PartyAuthorize.$cache.removeAll();
				page.models.PartyAuthorize.query(function (data) {
					$scope.partyAuth = data;

					angular.forEach($scope.partyAuth.children, function (party) {
						if (!party.authorized) {
							if (!actionMessages[party.id]) {
								actionMessages[party.id] = {
									party: party,
									message: page.messages.add({
										type: 'yellow',
										closeable: true,
										body: page.$compile('<span>' + page.constants.message('auth.pending.notice', party.party.name) + ' <a href="" ng-click="openMessageChild(' + party.id + ')">Manage</a>.</span>')($scope)
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

		$scope.openMessageChild = function (id) {
			page.messages.remove(actionMessages[id].message);
			$scope.openAuthChild(actionMessages[id].party);
		};

		$scope.$on('$destroy', function () {
			angular.forEach(actionMessages, function (bundle) {
				page.messages.remove(bundle.message);
			});
		});

		//

		$scope.resetAuth = {};

		$scope.currentAuthParent = undefined;
		$scope.currentAuthChild = undefined;

		var childWatch = undefined;

		//

		$scope.openAuthChild = function (child) {
			if (!child.force && !page.auth.hasAccess('ADMIN', $scope.party)) {
				return;
			}

			if (!child.force) {
				$scope.resetAuthChild(child);
			}

			$scope.currentAuthChild = child;
			page.display.scrollTo('network-child-' + child.id);
		};

		$scope.resetAuthChild = function (child) {
			if (angular.isDefined($scope.currentAuthChild) && angular.isDefined($scope.resetAuth)) {
				angular.extend($scope.currentAuthChild, $scope.resetAuth);
			}

			if (angular.isFunction(childWatch)) {
				childWatch();
			}

			if (!angular.isDefined(child)) {
				return;
			}

			$scope.resetAuth = {
				direct: child.direct,
				inherit: child.inherit,
				expires: child.expires,
				id: child.id
			};

			/* XXX What's this for?  If the user doesn't have admin access to this party, they can't change anything anyway. */
			if (!page.auth.hasAccess('ADMIN', $scope.party)) {
				childWatch = $scope.$watch(function () {
					return [child.direct, child.inherit, child.expiration];
				}, function (newVal, oldVal) {
					if (newVal[0] != oldVal[0]) {
						child.inherit = Math.min(child.inherit, Math.max(child.direct, page.permission.DOWNLOAD));
					} else if (newVal[1] != oldVal[1]) {
						if (child.inherit > page.permission.DOWNLOAD)
							child.direct = Math.max(child.direct, child.inherit);
					}
				}, true);
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
				direct: page.permission.DOWNLOA,
			} : $scope.currentAuthChild;

			form.successFn = grantCancelFn;
			form.cancelFn = grantCancelFn;
			form.denySuccessFn = grantCancelFn;
			event.stopPropagation();
		});

		//

		$scope.openAuthParent = function (parent) {
			if (!page.auth.hasAccess('ADMIN', $scope.party) && !parent.force) {
				return;
			}

			if (parent.force) {
				$scope.resetAuthChild(parent);
			}

			$scope.currentAuthParent = parent;
			page.display.scrollTo('network-parent-' + parent.id);
		};

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

		$scope.remoteAction = function (apply) {
			var request = {
				party: page.auth.user,
				force: true,
				remote: true,
				id: page.auth.user.id,
				direct: page.permission.DOWNLOAD,
				inherit: page.permission.DOWNLOAD
			};

			if (apply) {
				$scope.partyAuth.children[page.auth.user.id] = request;
				$scope.openAuthChild(request);
			} else {
				$scope.partyAuth.parents[page.auth.user.id] = request;
				$scope.openAuthParent(request);
			}
		};

		//

		var isAdmin = function () {
			return page.auth.hasAccess('ADMIN', $scope.party);
		};

		var isForeign = function () {
			return (!$scope.partyAuth.parents[page.auth.user.id] && !$scope.partyAuth.children[page.auth.user.id]);
		};

		$scope.showRegion = function (parents) {
			if (parents) {
				return !$.isEmptyObject($scope.partyAuth.parents) || isAdmin() || isForeign();
			}
			else {
				return !$.isEmptyObject($scope.partyAuth.children) || isAdmin() || isForeign();
			}
		};

		$scope.showRemote = function (parents) {
			if (parents) {
				return $scope.currentAuthParent.remote;
			}
			else {
				return $scope.currentAuthChild.remote;
			}
		};

		$scope.showExtended = function (parents, party) {
			if (parents) {
				return isAdmin() && $scope.currentAuthParent != party;
			}
			else {
				return isAdmin() && $scope.currentAuthChild != party;
			}
		};

		$scope.showAdd = function () {
			return isAdmin() || isForeign();
		};

		$scope.showSearch = function () {
			return isAdmin();
		};
	}
]);
