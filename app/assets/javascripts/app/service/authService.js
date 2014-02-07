define(['app/config/module'], function (module) {
	'use strict';

	module.factory('AuthService', ['$rootScope', '$location', '$cookieStore', '$http', '$route', '$cacheFactory', function ($rootScope, $location, $cookieStore, $http, $route, $cacheFactory) {
		var authService = {};

		//

		authService.user = undefined;
		authService.userUpdated = undefined;

		var updateUser = function (user) {
			var reload = false;

			authService.userUpdated = new Date();

			if (user) {
				if (angular.isDefined(user.superuser) && user.superuser > 0)
					user.superuser = new Date(Date.now() + user.superuser);
				else
					user.superuser = false;

				if (authService.user && (!!user.superuser != !!authService.user.superuser || user.id != authService.user.id))
					reload = true;
			}

			authService.user = user || undefined;

			if (reload) {
				$cacheFactory.get('$http').removeAll();
				$route.reload();
			}
		};

		$http
			.get('/api/user')
			.success(function (data) {
				updateUser(data);
			})
			.error(function () {
				updateUser(undefined);
			});

		//

		var levels = {};

		$rootScope.constant.$promise.then(function () {
			angular.forEach($rootScope.constant.data.permission, function (permission) {
				levels[permission.name] = permission.id;
			});

			levels['SUPER'] = 5;
		});

		var parseAuthLevel = function (level) {
			return angular.isString(level) ? levels[level] : level;
		};

		var parseUserAuth = function () {
			if (angular.isUndefined(authService.user))
				return parseAuthLevel('NONE');

			if (angular.isDate(authService.user.superuser) && authService.user.superuser > new Date())
				return parseAuthLevel('SUPER');

			return authService.user.access;
		};

		var parseUserAccess = function (object) {
			if (!object || !object.access)
				return parseAuthLevel('NONE');

			var level;

//			angular.forEach(object.access, function (access) {
//			});
		};

		//

		authService.hasAuth = function (level) {
			level = level.toUpperCase().split('!');

			return level.length == 1 ?
				parseUserAuth() >= parseAuthLevel(level.pop()) :
				parseUserAuth() < parseAuthLevel(level.pop());
		};

		authService.isAuth = function (level) {
			return parseUserAuth() == parseAuthLevel(level.toUpperCase().split('!').pop());
		};

		//

		authService.hasAccess = function (level, object) {
			level = level.toUpperCase().split('!');

			return level.length == 1 ?
				parseUserAccess(object) >= parseAuthLevel(level.pop()) :
				parseUserAccess(object) < parseAuthLevel(level.pop());
		};

		authService.isAccess = function (level, object) {
			return parseUserAccess(object) == parseAuthLevel(level.toUpperCase().split('!').pop());
		};

		//

		authService.showLogin = function () {
			$location.path('/login');
		};

		authService.login = function (data) {
			data = angular.extend({
				email: '',
				password: '',
				openid: ''
			}, data);

			$http
				.post('/api/user/login', data)
				.success(function (data) {
					updateUser(data);
					$location.path('/');
				})
				.error(function () {
					updateUser(undefined);
				});
		};

		authService.logout = function () {
			$http
				.post('/api/user/logout')
				.success(function (data) {
					updateUser(data);
					authService.showLogin();
				})
				.error(function () {
					$location.path('/');
				});
		};

		authService.showProfile = function () {
			$location.path('/party/' + authService.user.id);
		};

		//

		var enableSU = function () {
			$http
				.post('/api/user/superuser/on')
				.success(function (data) {
					updateUser(data);
				})
				.error(function () {
					console.log('bad!');
				});
		};

		var disableSU = function () {
			$http
				.post('/api/user/superuser/off')
				.success(function (data) {
					updateUser(data);
				})
				.error(function () {
					console.log('bad!');
				});
		};

		authService.toggleSU = function (state) {
			if ((angular.isDefined(state) && !state) || authService.isAuth('SUPER'))
				disableSU();
			else
				enableSU();
		};

		//

		return authService;
	}]);
});
