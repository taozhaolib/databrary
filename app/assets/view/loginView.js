'use strict';

module.controller('loginView', [
  '$scope', 'pageService', function ($scope, page) {
    page.display.title = page.constants.message('page.title.login');

    var form;
    var loginWatch = $scope.$watch('loginForm', function () {
      form = $scope.loginForm;

      form.validator.client({
        email: {
          tips: page.constants.message('login.email.help'),
          errors: page.constants.message('login.email.error'),
        },
        password: {
          tips: page.constants.message('login.password.help'),
        },
      }, true);

      //

      loginWatch();
    });

    //

    $scope.method = 'databrary';
    $scope.loginData = {};

    //

    $scope.switchMethod = function (method) {
      $scope.method = method;
    };

    $scope.showMethodLink = function (method) {
      return $scope.method != method;
    };

    //

    $scope.submitForm = function () {
      page.models.party.login(angular.extend({
        email: '',
        password: '',
        openid: '',
      }, $scope.loginData), function (data) {
        form.validator.server({});

        page.auth.parseUser(data);

        if (page.auth.next) {
          page.$location.path(page.auth.next);
          page.auth.next = undefined;
        } else {
          page.$location.path('/');
        }
      }, function (res) {
        form.validator.server(res, true);
      });
    };
  }
]);
