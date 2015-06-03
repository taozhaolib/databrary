'use strict';

app.directive('loginForm', [
  'pageService',
  function (page) { return {
    restrict: 'E',
    templateUrl: 'party/loginForm.html',
    link: function ($scope) {
      var form = $scope.loginForm;

      form.data = {};

      form.submit = function () {
        form.$setSubmitted();
        page.models.Login.login(form.data).then(function () {
          form.validator.server({});
          form.$setUnsubmitted();
          page.router.back();
        }, function (res) {
          form.$setUnsubmitted();
          form.validator.server(res, true);
        });
      };

      form.validator.client({}, true);

      page.$timeout(function(){angular.element('#loginEmail').focus();},300);
    }
  }; }
]);
