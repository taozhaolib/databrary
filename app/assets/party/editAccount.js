'use strict';

app.directive('partyEditAccountForm', [
  'pageService', function (page) {
    var link = function ($scope) {
      var party = $scope.party;
      var form = $scope.partyEditAccountForm;

      function init() {
        form.data = {
          email: party.email,
        };
      }
      init();

      $scope.$watch('partyEditAccountForm.data.password.again', function () {
        form['password.again'].$setValidity('match', !form.data.password || form.data.password.once === form.data.password.again);
      });

      form.save = function () {
        page.messages.clear(form);
        party.save(form.data).then(
          function () {
            form.validator.server({});

            page.messages.add({
              type: 'green',
              body: page.constants.message('party.edit.profile.success'),
              owner: form
            });

            init();
            form.$setPristine();
          }, function (res) {
            form.validator.server(res);
          });
      };

      var validate = {};
      ['email', 'password', 'password.again', 'auth'].forEach(function (f) {
        validate[f] = {
          tips: page.constants.message('party.edit.' + f + '.help')
        };
      });
      validate.email.errors = page.constants.message('login.email.error');
      validate['password.again'].errors = page.constants.message('party.edit.password.again.error');
      form.validator.client(validate, true);
    };

    return {
      restrict: 'E',
      templateUrl: 'party/editAccount.html',
      link: link
    };
  }
]);
