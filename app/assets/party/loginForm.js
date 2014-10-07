'use strict';

app.directive('loginForm', [
  'pageService',
  function (page) { return {
    restrict: 'E',
    templateUrl: 'party/loginForm.html',
    link: function ($scope) {
      var form = $scope.loginForm;

      form.method = 'databrary';
      form.data = {};

      form.switchMethod = function (method) {
        $scope.method = method;
      };

      form.submit = function () {
        page.models.Login.login(form.data).then(function () {
          form.validator.server({});

          page.router.back();
        }, function (res) {
          form.validator.server(res, true);
        });
      };

      form.validator.client({}, true);

      page.$timeout(function(){angular.element('#loginEmail').focus();},300);
    }
  }; }
]);
