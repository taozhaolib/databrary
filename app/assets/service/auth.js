'use strict';

app.factory('authService', [
  '$location', '$route', 'messageService', 'constantService', 'modelService',
  function ($location, $route, messages, constants, models) {
    var auth = {};

    //

    auth.logout = function () {
      messages.clear(auth);
      models.Login.logout().then(function () {
        $location.url('/');
        $route.reload();

        messages.add({
          body: constants.message('logout.success'),
          type: 'yellow',
        });
      }, function (res) {
        $location.url('/');

        messages.add({
          body: constants.message('logout.error'),
          report: res,
          owner: auth,
          persist: true
        });
      });
    };

    //

    var enableSU = function (form) {
      models.Login.superuserOn({
          auth: form.auth
        }).then(function () {
          $route.reload();
        }, function (res) {
          messages.addError({
            report: res,
          });
        });
    };

    var disableSU = function () {
      models.Login.superuserOff().then(function () {
        $route.reload();
      }, function (res) {
        messages.addError({
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
