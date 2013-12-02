define([
	'app/modules/dbServices'
], function (db) {
	'use strict';

	db.factory('AuthService', ['$rootScope', '$cookieStore', '$http', function ($rootScope, $cookieStore, $http) {
		var authService = {};

		var nullUser = {
			access: 0
		};

		$rootScope.authUser = nullUser;

		var levels = {
			none: 0,
			view: 10,
			download: 20,
			edit: 30,
			admin: 40
		};

		//

		authService.logIn = function () { // TODO
			$http
				.post('/login')
				.success(function (data, status, headers, config) {
					$rootScope.authUser = data;
					broadcastAuthChange();
				})
				.error(function (data, status, headers, config) {
					$rootScope.authUser = nullUser;
					console.log('login failed');
				});
		};

		authService.logOut = function () { // TODO
			$http
				.get('/logout')
				.success(function (data, status, headers, config) {
					$rootScope.authUser = nullUser;
					broadcastAuthChange();
				})
				.error(function (data, status, headers, config) {
					console.log('logout failed');
				});
		};

		authService.isUser = function () {
			return $rootScope.authUser.access > levels['none'];
		};

		//

		authService.enableSU = function () { // TODO
			$http
				.get('/superuser/on')
				.success(function (data, status, headers, config) {
					console.log('good!');
					broadcastAuthChange();
				})
				.error(function (data, status, headers, config) {
					console.log('bad!');
				});
		};

		authService.disableSU = function () { // TODO
			$http
				.get('/superuser/off')
				.success(function (data, status, headers, config) {
					console.log('good!');
					broadcastAuthChange();
				})
				.error(function (data, status, headers, config) {
					console.log('bad!');
				});
		};

		authService.toggleSU = function () {
			if(authService.isSU())
				authService.disableSU();
			else
				authService.enableSU();
		};

		authService.isSU = function () { // TODO

		};

		//

		authService.hasAuth = function (level) {
			if (level.substr(0, 1) == '!')
				return $rootScope.authUser.access < levels[level.substr(1)];

			return $rootScope.authUser.access >= levels[level];
		};

		authService.hasAccess = function (level, obj) {
			// TODO: this is not correct. come back when you've got actual objects
			return obj.permission >= levels[level];
		};

		//

		authService.getAuthUser = function () {
			return $rootScope.authUser;
		};

		//

		var broadcastAuthChange = function () {
			$rootScope.$broadcast('authChange');
		};

		//

		return authService;
	}]);
});
