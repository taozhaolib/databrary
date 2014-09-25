'use strict';

module.factory('authService', [
  '$location', '$route', 'messageService', 'constantService', 'modelService', '$play',
  function ($location, $route, messages, constants, models, $play) {
    var auth = {};

    //

    auth.logout = function () {
      models.Login.logout().then(function () {
        $location.url('/');
	$route.reload();

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

    auth.hasToken = function () {
      return $play.object && $play.object.auth;
    };

    auth.getToken = function () {
      if (!auth.hasToken()) {
        return;
      }

      return $play.object;
    };

    auth.isPasswordPending = function () {
      return auth.hasToken() && !$play.object.reset;
    };

    //

    var enableSU = function (form) {
      models.Login.superuserOn({
	  auth: form.auth
        }).then(function () {
          messages.add({
            body: constants.message('superuser.on.success'),
            type: 'green',
            countdown: 2000
          });
	  $route.reload();
        }, function (res) {
          messages.addError({
            body: constants.message('superuser.on.error'),
            report: res,
          });
        });
    };

    var disableSU = function () {
      models.Login.superuserOff().then(function () {
        messages.add({
          body: constants.message('superuser.off.success'),
          type: 'green',
          countdown: 2000
        });
	$route.reload();
      }, function (res) {
        messages.addError({
          body: constants.message('superuser.off.error'),
          report: res,
        });
      });
    };

    auth.toggleSU = function (form) {
      if (form)
        enableSU(form);
      else
        disableSU();
    };

    //

    return auth;
  }
]);
