'use strict';

module.directive('loginForm', [
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

      form.validator.client({
        email: {
          tips: page.constants.message('login.email.help'),
          errors: page.constants.message('login.email.error'),
        },
        password: {
          tips: page.constants.message('login.password.help'),
        },
      }, true);

      page.$timeout(function(){angular.element('#loginEmail').focus();},300);
    }
  }; }
]);
