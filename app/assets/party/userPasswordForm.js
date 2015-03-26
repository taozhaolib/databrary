'use strict';

app.directive('userPasswordForm', [
  'pageService',
  function (page) {
    var link = function ($scope) {
      var form = $scope.userPasswordForm;

      form.data = {
        email: undefined,
      };

      //

      form.resetSuccessFn = undefined;

      form.reset = function () {
        page.messages.clear(form);
        page.models.Login.issuePassword($scope.userPasswordForm.data)
          .then(function () {
            form.validator.server({});

            page.messages.add({
              type: 'green',
              body: page.constants.message('reset.request.success', form.data.email),
              owner: form
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
