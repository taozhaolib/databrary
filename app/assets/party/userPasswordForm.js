'use strict';

module.directive('userPasswordForm', [
  'pageService', '$play',
  function (page, $play) {
    var link = function ($scope) {
      var form = $scope.userPasswordForm;
      var token;

      form.data = {
        token: undefined,
        auth: undefined,
        email: undefined,
        password: {
          once: undefined,
          again: undefined
        }
      };

      if (page.auth.hasToken()) {
        token = page.auth.getToken();
        form.data.token = token.id;
        form.data.auth = token.auth;
      }

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

            if (angular.isFunction(form.resetSuccessFn)) {
              form.resetSuccessFn();
            }
          }, function (res) {
            form.validator.server(res);
          });
      };

      //

      form.saveSuccessFn = undefined;

      form.save = function () {
	page.models.Login.passwordToken(token.party, $scope.userPasswordForm.data)
          .then(function () {
            form.validator.server({});

            form.messages.add({
              type: 'green',
              countdown: 3000,
              body: page.constants.message('reset.save.success', form.data.email)
            });

            $play.object = null;

            if (angular.isFunction(form.saveSuccessFn)) {
              form.saveSuccessFn();
            }
          }, function (res) {
            form.validator.server(res);

            $play.object = null;
          });
      };

      //

      form.ready = function () {
        if (form.data.token) {
          return form.$dirty && form.$valid && form.data.password.once &&
            form.data.password.once === form.data.password.again;
        }
        else {
          return form.$dirty && form.$valid && form.data.email;
        }
      };

      //

      form.validator.client({
        email: {
          tips: page.constants.message('reset.email.help'),
          errors: page.constants.message('login.email.error'),
        },
        'password.once': {
          tips: page.constants.message('reset.once.help'),
        },
        'password.again': {
          tips: page.constants.message('reset.again.help'),
        },
      }, true);

      //

      $scope.$emit('userPasswordForm-init', form);
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
