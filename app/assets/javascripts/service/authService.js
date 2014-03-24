define(['config/module'], function (module) {
	'use strict';

	module.factory('AuthService', ['$rootScope', '$location', '$http', '$route', '$cacheFactory', 'TypeService', '$window', '$q', function ($rootScope, $location, $http, $route, $cacheFactory, typeService, $window, $q) {
		var authService = {};

		//

		authService.user = undefined;
		authService.userUpdated = undefined;

		var updateUser = function (user) {
			var reload = true;

			authService.userUpdated = new Date();

			if (user) {
				if (angular.isDefined(user.superuser) && user.superuser > 0)
					user.superuser = new Date(Date.now() + user.superuser);
				else
					user.superuser = false;

				if(!user || user != authService.user)

				if(authService.user && user && authService.user.id == user.id &&
					!!authService.user.superuser == !!user.superuser)
					reload = false;
			}

			authService.user = user || undefined;

			if (reload) {
				$cacheFactory.get('$http').removeAll();
				$route.reload();
			}
		};

		var deferred = $q.defer();
		authService.$promise = deferred.promise;

		authService.updateUser = function (user) {
			if(user)
				return updateUser(user);

			$http
				.get('/api/user')
				.success(function (data) {
					if(data.id == -1)
						updateUser(undefined);
					else
						updateUser(data);

					deferred.resolve();
				})
				.error(function () {
					updateUser(undefined);

					deferred.resolve();
				});
		};

		authService.updateUser();

		//

		var levels = {};

		$rootScope.$watch('constant', function () {
			$rootScope.constant.$promise.then(function () {
				angular.forEach($rootScope.constant.data.permission, function (permission) {
					levels[permission.name] = permission.id;
				});

				levels['SUPER'] = 5;
			});
		});

		var parseAuthLevel = function (level) {
			return angular.isString(level) ? levels[level] : level;
		};

		var parseUserAuth = function (object) {
			if (angular.isUndefined(authService.user) || authService.user.id == -1)
				return -1;

			if (angular.isDate(authService.user.superuser) && authService.user.superuser > new Date())
				return parseAuthLevel('SUPER');

			if(angular.isObject(object) && object.permission)
				return object.permission;

			return authService.user.access;
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
				parseUserAuth(object) >= parseAuthLevel(level.pop()) :
				parseUserAuth(object) < parseAuthLevel(level.pop());
		};

		authService.isAccess = function (level, object) {
			return parseUserAuth(object) == parseAuthLevel(level.toUpperCase().split('!').pop());
		};

		//

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

					if (authService.next)
						$location.path(authService.next);
					else
						$location.path('/');
				})
				.error(function () {
					updateUser(undefined);
				});
		};

		authService.tryLogin = function (next, current) {
			authService.next = $location.url();

			$location.url('/login');
		};

		authService.logout = function () {
			$http
				.post('/api/user/logout')
				.success(function (data) {
					updateUser(data);
					$location.url('/login');
				})
				.error(function () {
					$location.path('/');
				});
		};

		authService.showProfile = function () {
			$location.path('/party/' + authService.user.id);
		};

		//

		authService.isLoggedIn = function () {
			return angular.isDefined(authService.user) && authService.user.id != -1;
		};

		authService.hasToken = function () {
			return $window.$play && $window.$play.object && typeService.isToken($window.$play.object);
		};

		authService.getToken = function () {
			if(!authService.hasToken())
				return;

			return $window.$play.object;
		};

		authService.isPasswordReset = function () {
			return authService.hasToken() && $window.$play.object.reset;
		};

		authService.isPasswordPending = function () {
			return authService.hasToken() && !$window.$play.object.reset;
		};

		authService.isUnauthorized = function () {
			return authService.isAuth('NONE');
		};

		//

		var enableSU = function (form) {
			$http
				.post('/api/user/superuser/on', {
					auth: form.auth
				})
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

		authService.toggleSU = function (form) {
			if (angular.isDefined(form))
				enableSU(form);
			else
				disableSU();
		};

		//

		return authService;
	}]);
});
