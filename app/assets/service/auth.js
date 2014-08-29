'use strict';

module.factory('authService', [
  '$rootScope',
  '$location',
  '$route',
  '$cacheFactory',
  'typeService',
  'messageService',
  'constantService',
  'modelService',
  'playService',
  function ($rootScope, $location, $route, $cacheFactory, types, messages, constants, models, play) {
    var auth = {};

    //

    auth.user = undefined;

    auth.parseUser = function (user) {
      var reload = true;

      if (!angular.isObject(user))
	user = undefined;

      if (user) {
        user.superuser = !!user.superuser;

        if (auth.user && auth.user.id === user.id && auth.user.superuser == user.superuser) {
          reload = false;
        }
      } else if (!user && !auth.user) {
        reload = false;
      }

      auth.user = user;

      if (reload) {
        $cacheFactory.removeAll();
        $route.reload();
      }
    };

    auth.updateUser = function (user) {
      if (user) {
        return auth.parseUser(user);
      }

      models.Login.get().then(auth.parseUser,
	function () {
	  auth.parseUser(undefined);
	});
    };

    auth.updateUser(play.user);

    //

    var parseUserAuth = function (object) {
      if (auth.user && auth.user.superuser) {
        return constants.data.permissionName.SUPER;
      }

      if (angular.isObject(object) && 'permission' in object) {
        return object.permission;
      }

      if (!(auth.user && 'access' in auth.user))
	return -1;

      return auth.user.access;
    };

    //

    auth.hasAuth = function (level) {
      return parseUserAuth() >= level;
    };

    //

    auth.hasAccess = function (level, object) {
      return parseUserAuth(object) >= level;
    };

    //

    auth.logout = function () {
      models.Login.logout().then(function () {
        auth.parseUser();
        $location.url('/');

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
      return !!(auth.user && auth.user.id != constants.data.party.NOBODY);
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
      models.Login.superuserOn({
	  auth: form.auth
        }).then(function (data) {
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
      models.Login.superuserOff().then(function (data) {
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
