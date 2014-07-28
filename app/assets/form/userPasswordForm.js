'use strict';

module.directive('userPasswordForm', [
  'pageService', function (page) {
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

      form.resetFn = undefined;
      form.resetSuccessFn = undefined;
      form.resetErrorFn = undefined;

      form.reset = function () {
	if (angular.isFunction(form.resetFn)) {
	  form.resetFn(form);
	}

	page.$http
	  .post('/password', $scope.userPasswordForm.data)
	  .success(function () {
	    form.validator.server({});

	    form.messages.add({
	      type: 'green',
	      countdown: 3000,
	      body: page.constants.message('reset.request.success', form.data.email)
	    });

	    if (angular.isFunction(form.resetSuccessFn)) {
	      form.resetSuccessFn(form, arguments);
	    }
	  })
	  .error(function (data, status) {
	    form.validator.server({
	      data: data,
	      status: status,
	    });

	    if (angular.isFunction(form.resetErrorFn)) {
	      form.resetErrorFn(form, arguments);
	    }
	  });
      };

      //

      form.saveFn = undefined;
      form.saveSuccessFn = undefined;
      form.saveErrorFn = undefined;

      form.save = function () {
	if (angular.isFunction(form.saveFn)) {
	  form.saveFn(form);
	}

	page.$http
	  .post('/api/party/' + token.party + '/password', $scope.userPasswordForm.data)
	  .success(function (data) {
	    form.validator.server({});

	    form.messages.add({
	      type: 'green',
	      countdown: 3000,
	      body: page.constants.message('reset.save.success', form.data.email)
	    });

	    if (angular.isFunction(form.saveSuccessFn)) {
	      form.saveSuccessFn(form, arguments);
	    }

	    page.$window.$play.object = null;
	    page.auth.updateUser(data);
	  })
	  .error(function (data, status) {
	    form.validator.server({
	      data: data,
	      status: status,
	    });

	    if (angular.isFunction(form.saveErrorFn)) {
	      form.saveErrorFn(form, arguments);
	    }

	    page.$window.$play.object = null;
	  });
      };

      //

      form.ready = function () {
	if (form.data.token) {
	  return form.$dirty && form.$valid && form.data.password.once &&
	    form.data.password.once == form.data.password.again;
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

      page.events.talk('userPasswordForm-init', form, $scope);
    };

    //

    return {
      restrict: 'E',
      templateUrl: 'userPasswordForm.html',
      scope: false,
      replace: true,
      link: link
    };
  }
]);
