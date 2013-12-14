define(['app/config/module'], function (module) {
	'use strict';

	module.factory('AuthService', ['$rootScope', '$location', '$cookieStore', '$http', '$route', function ($rootScope, $location, $cookieStore, $http, $route) {
		var authService = {};

		//

		authService.user = undefined;
		authService.userUpdated = undefined;

		var updateUser = function (user) {
			var reload = false;

			if (angular.isUndefined(user))
				return authService.user = user;

			if (angular.isDefined(user.superuser) && user.superuser > 0)
				user.superuser = new Date(user.superuser);
			else
				user.superuser = false;

			if (!!user.superuser != !!authService.user)
				reload = true;

			authService.user = user;
			authService.userUpdated = new Date();

			if (reload)
				$route.reload();
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

		var levels = {
			NONE: 0,
			VIEW: 1,
			DOWNLOAD: 2,
			EDIT: 3,
			ADMIN: 4,
			SUPER: 5
		};

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

		var parseUserAccess = function () {
// TODO...........................................
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

		authService.hasAccess = function (level) {
			level = level.toUpperCase().split('!');

			return level.length == 1 ?
				parseUserAccess() >= parseAuthLevel(level.pop()) :
				parseUserAccess() < parseAuthLevel(level.pop());
		};

		authService.isAccess = function (level) {
			return parseUserAccess() == parseAuthLevel(level.toUpperCase().split('!').pop());
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

			console.log(data);

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
