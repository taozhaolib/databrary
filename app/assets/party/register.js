'use strict';

app.controller('party/register', [
  '$scope', 'pageService', 'user', 'token',
  function ($scope, page, user, token) {
    $scope.page = page;
    page.display.title = page.constants.message('register.title');

    $scope.auth = {};

    var initStep = {
      create: function (step) {
        var form = $scope.registerForm = step.$scope.registerForm;
        form.data = {};
        form.sent = false;

        var validate = {};
        ['prename', 'sortname', 'email', 'affiliation'].forEach(function (f) {
          validate[f] = {
            tips: page.constants.message('party.edit.' + f + '.help')
          };
        });
        validate.email.errors = page.constants.message('register.email.error');
        form.validator.client(validate, true);
      },

      agreement: function (step) {
        step.$scope.proceed = function () {
          page.models.Login.register($scope.registerForm.data)
            .then(function () {
              $scope.registerForm.sent = true;
              $scope.proceed();
            }, function (res) {
              page.messages.addError({
                body: page.constants.message('error.generic'),
                report: res
              });
            });
        };
      },

      email: function (/*step*/) {
      },

      password: function (step) {
        var form = $scope.passwordForm = step.$scope.passwordForm;
        form.sent = false;
        if (token)
          form.data = {
            token: token.id,
            auth: token.auth
          };

        form.save = function () {
          page.messages.clear(form);
          page.models.Login.passwordToken(token.party, form.data)
            .then(function () {
              form.validator.server({});

              page.messages.add({
                type: 'green',
                body: page.constants.message('reset.save.success', form.data.email),
                owner: form
              });

              form.sent = true;
              page.$location.url(page.router.register());
            }, function (res) {
              form.validator.server(res);
            });
        };

        form.validator.client({
          'password.once': {
            tips: page.constants.message('reset.once.help'),
          },
          'password.again': {
            tips: page.constants.message('reset.again.help'),
          },
        }, true);
      },

      agent: function (step) {
        var form = $scope.authSearchForm = step.$scope.authSearchForm;
        form.data = {};

        form.selectFn = function (found) {
          $scope.auth.party = found;
          delete $scope.auth.query;
          $scope.proceed();
        };

        form.notFoundFn = function (query) {
          delete $scope.auth.party;
          $scope.auth.query = query;
          $scope.proceed();
        };
      },

      request: function (step) {
        var form = $scope.authApplyForm = step.$scope.authApplyForm;
        form.sent = false;

        form.successFn = function () {
          form.sent = true;
          updateUserAuth();
        };

        form.cancelFn = function () {
          delete $scope.auth.party;
          delete $scope.auth.query;
          $scope.proceed();
        };
      },

      pending: function () {
      }
    };

    $scope.registerStep = function (step) {
      initStep[step.name](step);
      if (step.name === 'pending')
        $scope.proceed();
    };

    function updateUserAuth() {
      user.get(['parents']).then(function () {
        $scope.proceed();
      }, function (res) {
        page.messages.addError({
          body: page.constants.message('register.authquery.error'),
          report: res,
        });
      });
    }

    $scope.proceed = function () {
      var s = {};
      if (!page.models.Login.isLoggedIn())
        if (!token)
          if (!$scope.registerForm.sent) {
            s.create = true;
            s.agreement = $scope.registerForm.$dirty && $scope.registerForm.$valid;
          } else
            s.email = true;
        else
          s.password = !$scope.passwordForm.sent;
      else if (!(user.parents && user.parents.length) && !$scope.authApplyForm.sent) {
        s.agent = true;
        s.request = $scope.auth.party || $scope.auth.query;
      } else if (!page.models.Login.isAuthorized())
        s.pending = true;

      var a;
      for (var si = $scope.steps.length-1; si >= 0; si --) {
        var step = $scope.steps[si];
        step.complete = !!a;
        if ((step.allow = !!s[step.name]))
          a = a || step;
      }
      if (a)
        $scope.activateStep(a);
      else
        page.$location.url('/');
    };
  }
]);
