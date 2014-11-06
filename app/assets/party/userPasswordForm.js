'use strict';

app.directive('userPasswordForm', [
  'pageService',
  function (page) {
    var link = function ($scope) {
      var form = $scope.userPasswordForm;
      var token = $scope.token;

      form.data = {
        email: undefined,
      };

      //

      form.resetSuccessFn = undefined;

      form.reset = function () {
        page.models.Login.issuePassword($scope.userPasswordForm.data)
          .then(function () {
            form.validator.server({});

            form.messages.add({
              type: 'green',
              countdown: 3000,
              body: page.constants.message('reset.request.success', form.data.email)
            });

            page.$location.url(page.router.index());
          }, function (res) {
            form.validator.server(res);
          });
      };

      form.validator.client({
        email: {
          tips: page.constants.message('reset.email.help'),
          errors: page.constants.message('login.email.error'),
        },
      }, true);
    };

    //

    return {
      restrict: 'E',
      templateUrl: 'party/userPasswordForm.html',
      scope: false,
      replace: true,
      link: link
    };
  }
]);
