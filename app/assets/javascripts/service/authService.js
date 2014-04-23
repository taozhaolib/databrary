module.factory('authService', [
	'$rootScope',
	'$location',
	'$route',
	'cacheService',
	'typeService',
	'$window',
	'$q',
	'messageService',
	'constantService',
	'routerService',
	'Party',
	function ($rootScope, $location, $route, cache, typeService, $window, $q, messages, constants, router, Party) {
		var auth = {};

		//

		auth.user = undefined;

		var parseUser = function (user) {
			var reload = true;

			if (user) {
				if (angular.isDefined(user.superuser) && user.superuser > 0)
					user.superuser = new Date(Date.now() + user.superuser);
				else
					user.superuser = false;

				if (auth.user && auth.user.id === user.id && !!auth.user.superuser === !!user.superuser)
					reload = false;
			}

			auth.user = user || undefined;

			if (reload) {
				cache().reload();
				$route.reload();
			}
		};

		var deferred = $q.defer();
		auth.$promise = deferred.promise;

		auth.updateUser = function (user) {
			if (user) {
				parseUser(user);
				return deferred.resolve();
			}

			Party.user(function (data) {
				if (data.id == -1 || angular.isString(data))
					parseUser(undefined);
				else
					parseUser(data);

				deferred.resolve();
			}, function () {
				parseUser(undefined);

				deferred.resolve();
			});
		};

		auth.updateUser();

		//

		var levels = {};

		constants.$promise.then(function () {
			angular.forEach(constants.data.permission, function (permission) {
				levels[permission.name] = permission.id;
			});

			levels['SUPER'] = 5;
		});

		var parseAuthLevel = function (level) {
			return $.isNumeric(level) ? parseInt(level) :
				angular.isString(level) ? levels[level.toUpperCase()] : -1;
		};

		var parseUserAuth = function (object) {
			if (!auth.user || !auth.user.id || auth.user.id == -1)
				return -1;

			if (angular.isDate(auth.user.superuser) && auth.user.superuser > new Date())
				return parseAuthLevel('SUPER');

			if (angular.isObject(object) && object.permission)
				return object.permission;

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

		auth.login = function (data) {
			Party.login(angular.extend({
				email: '',
				password: '',
				openid: ''
			}, data), function (data) {
				parseUser(data);

				if (auth.next) {
					$location.path(auth.next);
					auth.next = undefined;
				} else {
					$location.path('/');
				}
			}, function () {
				parseUser(undefined);

				messages.add({
					body: constants.message('login.error'),
					type: 'red',
					countdown: 3000
				});
			});
		};

		auth.tryLogin = function () {
			auth.next = $location.url();
			$location.url(router.login());
		};

		auth.logout = function () {
			Party.logout(function (data) {
				parseUser(data);
				$location.url('/login');

				messages.add({
					body: constants.message('logout.success'),
					type: 'yellow',
					countdown: 3000
				});
			}, function () {
				$location.url('/');

				messages.add({
					body: constants.message('logout.error'),
					type: 'red',
					countdown: 3000
				});
			});
		};

		auth.showProfile = function () {
			$location.path(router.profile());
		};

		//

		auth.isLoggedIn = function () {
			return auth.user && auth.user.id && auth.user.id != -1;
		};

		auth.hasToken = function () {
			return $window.$play && $window.$play.object && typeService.isToken($window.$play.object);
		};

		auth.getToken = function () {
			if (!auth.hasToken())
				return;

			return $window.$play.object;
		};

		auth.isPasswordReset = function () {
			return auth.hasToken() && $window.$play.object.reset;
		};

		auth.isPasswordPending = function () {
			return auth.hasToken() && !$window.$play.object.reset;
		};

		auth.isUnauthorized = function () {
			return auth.isLoggedIn() && auth.isAuth('NONE');
		};

		auth.isAuthorized = function () {
			return auth.isLoggedIn() && auth.hasAuth('VIEW');
		};

		//

		var enableSU = function (form) {
			Party.superuserOn({
					auth: form.auth
				},
				function (data) {
					parseUser(data);

					messages.add({
						body: constants.message('superuser.on.success'),
						type: 'green',
						countdown: 2000
					});
				}, function (res) {
					messages.addError({
						body: constants.message('superuser.on.error'),
						errors: res.errors,
						status: res.status
					});
				});
		};

		var disableSU = function () {
			Party.superuserOff(function (data) {
				parseUser(data);

				messages.add({
					body: constants.message('superuser.off.success'),
					type: 'green',
					countdown: 2000
				});
			}, function (res) {
				messages.addError({
					body: constants.message('superuser.off.error'),
					errors: res.data,
					status: res.status
				});
			});
		};

		auth.toggleSU = function (form) {
			if (angular.isDefined(form))
				enableSU(form);
			else
				disableSU();
		};

		//

		return auth;
	}
]);
