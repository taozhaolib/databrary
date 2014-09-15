'use strict';

module.controller('registerView', [
  '$scope', 'pageService', function ($scope, page) {
    page.display.title = page.constants.message('page.title.register');

    $scope.forms = {};

    var initStep = {
      create: function (step) {
	var form = $scope.registerForm = step.$scope.registerForm;
        form.data = {};
        form.sent = false;

	var validate = {};
	['name', 'email', 'affiliation'].forEach(function (f) {
	  validate[f] = {
	    tips: page.constants.message('register.' + f + '.help')
	  };
	});
	validate.email.errors = page.constants.message('register.email.error');
        form.validator.client(validate, true);
      },

      agreement: function (step) {
        step.$scope.proceed = function () {
          $scope.registerForm.data.agreement = true;

	  page.models.Login.register($scope.registerForm.data)
            .then(function () {
              $scope.registerForm.sent = true;
              proceed();
            }, function (errors, status) {
              page.messages.addError({
                body: page.constants.message('error.generic'),
                errors: errors,
                status: status
              });
            });
        };
      },

      email: function (step) {
      },

      password: function (step) {
        var form = $scope.userPasswordForm = step.$scope.userPasswordForm;
        form.sent = false;

        form.saveSuccessFn = function () {
          form.sent = true;
	  page.$location.url(page.router.register());
          proceed();
        };
      },

      agent: function (step) {
        var form = $scope.authSearchForm = step.$scope.authSearchForm;
        form.data = {};

        form.selectFn = function (found) {
          form.data.party = found;
          $scope.authApplyForm.other =  {
            id: found.id,
            party: found
	  };
          proceed();
        };

        form.notFoundFn = function (query) {
          form.data.party = true;
          proceed();
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
          $scope.authSearchForm.data.party = undefined;
          form.other = undefined;
	  proceed();
        };

        step.$scope.proceed = function () {
	  page.models.Login.user.authorizeSearch(true, {
	      notfound: true,
	      // name: info.data.query,
	      // info: info.data.info
	    }).then(function () {
              form.successFn();
            }, function (errors, status) {
              page.messages.addError({
                body: page.constants.message('error.generic'),
                errors: errors,
                status: status
              });
            });
        };
      },

      pending: function () {
      }
    };

    $scope.registerStep = function (step) {
      initStep[step.name](step);
    };

    function updateUserAuth() {
      page.models.Login.user.get(['parents', 'children']).then(function (data) {
        proceed();
      }, function (res) {
        page.messages.addError({
          body: page.constants.message('register.authquery.error'),
          report: res,
        });
      });
    }

    function proceed() {
      var s;
      if (!page.models.Login.isLoggedIn())
	if (!page.auth.isPasswordPending())
	  if (!$scope.registerForm.sent) {
	    s.create = true;
	    s.agreement = $scope.registerForm.$valid;
	  } else
	    s.email = true;
	else
	  s.password = !$scope.userPasswordForm.sent;
      else if (!(page.models.Login.user.parents && page.models.Login.user.parents.length) && !$scope.authApplyForm.sent) {
	s.agent = true;
	s.request = $scope.authSearchForm.data.party;
      } else if (!page.models.Login.isAuthorized())
	s.pending = true;

      var c = false;
      for (var si = $scope.steps.length-1; si >= 0; si --) {
	var step = $scope.steps[si];
	step.complete = c;
	if ((step.allow = s[step.name]) && !c) {
	  $scope.activateStep(step);
	  c = true;
	}
      }
      if (!c)
	;
    }
  }
]);
