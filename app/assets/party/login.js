'use strict';

app.controller('party/login', [
  '$location', 'displayService', 'constantService', 'modelService',
  function ($location, display, constants, models) {
    display.title = constants.message('login.title');
    if (models.Login.isLoggedIn())
      $location.url('/');
  }
]);
