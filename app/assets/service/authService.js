module.factory('authService', [
	'$rootScope',
	'$location',
	'$route',
	'$cacheFactory',
	'typeService',
	'messageService',
	'constantService',
	'routerService',
	'party',
	'playService',
	function ($rootScope, $location, $route, $cacheFactory, types, messages, constants, router, party, play) {
		var auth = {};

		//

		auth.user = undefined;

		auth.parseUser = function (user) {
			var reload = true;

			if (user) {
				user.superuser = !!user.superuser;

				if (auth.user && auth.user.id === user.id && !auth.user.superuser === !user.superuser) {
					reload = false;
				}
			} else if (!user && !auth.user) {
				reload = false;
			}

			auth.user = user || undefined;

			if (reload) {
				$cacheFactory.removeAll();
				$route.reload();
			}
		};

		auth.updateUser = function (user) {
			if (user) {
				return auth.parseUser(user);
			}

			party.user(function (data) {
				if (data.id == -1 || angular.isString(data)) {
					auth.parseUser(undefined);
				}
				else {
					auth.parseUser(data);
				}
			}, function () {
				auth.parseUser(undefined);
			});
		};

		auth.updateUser(play.user);

		//

		var parseAuthLevel = function (level) {
			return $.isNumeric(level) ? parseInt(level) :
				angular.isString(level) ? constants.data.permissionName[level.toUpperCase()] : -1;
		};

		var parseUserAuth = function (object) {
			if (!auth.user || !auth.user.id || auth.user.id == -1) {
				return -1;
			}

			if (auth.user.superuser) {
				return constants.data.permissionName.SUPER;
			}

			if (angular.isObject(object) && object.permission) {
				return object.permission;
			}

			return auth.user.access;
		};

		//

		auth.hasAuth = function (level) {
			return parseUserAuth() >= parseAuthLevel(level);
		};

		auth.isAuth = function (level) {
			return parseUserAuth() == parseAuthLevel(level);
		};

		//

		auth.hasAccess = function (level, object) {
			return parseUserAuth(object) >= parseAuthLevel(level);
		};

		auth.isAccess = function (level, object) {
			return parseUserAuth(object) == parseAuthLevel(level);
		};

		//

		auth.logout = function () {
			party.logout(function (data) {
				auth.parseUser(data);
				$location.url('/login');

				messages.add({
					body: constants.message('logout.success'),
					type: 'yellow',
					countdown: 3000
				});
			}, function (res) {
				$location.url('/');

				messages.add({
					body: constants.message('logout.error'),
					report: res,
				});
			});
		};

		auth.showProfile = function () {
			$location.path(router.profile());
		};

		//

		auth.isLoggedIn = function () {
			return !!(auth.user && auth.user.id && auth.user.id != -1);
		};

		auth.hasToken = function () {
			return play.object && types.isToken(play.object);
		};

		auth.getToken = function () {
			if (!auth.hasToken()) {
				return;
			}

			return play.object;
		};

		auth.isPasswordReset = function () {
			return auth.hasToken() && play.object.reset;
		};

		auth.isPasswordPending = function () {
			return auth.hasToken() && !play.object.reset;
		};

		auth.isAuthorized = function () {
			return auth.isLoggedIn() && auth.hasAuth(constants.data.permissionName.PUBLIC);
		};

		auth.isUnauthorized = function () {
			return !auth.isAuthorized();
		};

		//

		var enableSU = function (form) {
			party.superuserOn({
					auth: form.auth
				},
				function (data) {
					auth.parseUser(data);

					messages.add({
						body: constants.message('superuser.on.success'),
						type: 'green',
						countdown: 2000
					});
				}, function (res) {
					messages.addError({
						body: constants.message('superuser.on.error'),
						report: res,
					});
				});
		};

		var disableSU = function () {
			party.superuserOff(function (data) {
				auth.parseUser(data);

				messages.add({
					body: constants.message('superuser.off.success'),
					type: 'green',
					countdown: 2000
				});
			}, function (res) {
				messages.addError({
					body: constants.message('superuser.off.error'),
					report: res,
				});
			});
		};

		auth.toggleSU = function (form) {
			if (angular.isDefined(form)) {
				enableSU(form);
			} else {
				disableSU();
			}
		};

		//

		return auth;
	}
]);
