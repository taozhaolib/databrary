'use strict';

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

		auth.everybody = constants.data.everybody;
		auth.user = auth.everybody;

		auth.parseUser = function (user) {
			var reload = true;

			if (user) {
				user.superuser = !!user.superuser;

				if (auth.user.id === user.id && auth.user.superuser == user.superuser) {
					reload = false;
				}
			} else if (!user && !auth.user) {
				reload = false;
			}

			auth.user = user || auth.everybody;

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
				auth.parseUser(angular.isString(data) ? auth.everybody : data);
			}, function () {
				auth.parseUser(auth.everybody);
			});
		};

		auth.updateUser(play.user);

		//

		var parseAuthLevel = function (level) {
			return $.isNumeric(level) ? parseInt(level) :
				angular.isString(level) ? constants.data.permissionName[level.toUpperCase()] : -1;
		};

		var parseUserAuth = function (object) {
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

		//

		auth.hasAccess = function (level, object) {
			return parseUserAuth(object) >= parseAuthLevel(level);
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

		//

		auth.isLoggedIn = function () {
			return auth.user.id !== auth.everybody.id;
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

		auth.isPasswordPending = function () {
			return auth.hasToken() && !play.object.reset;
		};

		auth.isAuthorized = function () {
			return auth.isLoggedIn() && auth.hasAuth(constants.data.permissionName.PUBLIC);
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
