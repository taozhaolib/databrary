'use strict';

module.directive('partyEditAccountForm', [
  'pageService', function (page) {
    var link = function ($scope) {
      var form = $scope.partyEditAccountForm;

      form.data = {};
      form.authors = [];

      form.saveFn = undefined;
      form.successFn = undefined;
      form.errorFn = undefined;

      //

      form.init = function (party) {
        form.party = form.party || party;
        form.data = {
          email: party.email,
        };
      };

      $scope.$watch('partyEditAccountForm.data.password.again', function () {
        form['password.again'].$setValidity('match', !form.data.password || form.data.password.once == form.data.password.again);
      });

      //

      form.save = function () {
        if (angular.isFunction(form.saveFn)) {
          form.saveFn(form);
        }

	form.party.save(form.data).then(
          function (res) {
            form.validator.server({});

            form.messages.add({
              type: 'green',
              countdown: 3000,
              body: page.constants.message('party.edit.profile.success'),
            });

            if (angular.isFunction(form.successFn)) {
              form.successFn(form, res);
            }

            form.$setPristine();
            form.clearPasswordFields();
          }, function (res) {
            form.validator.server(res);
            page.display.scrollTo(form.$element);

            if (angular.isFunction(form.errorFn)) {
              form.errorFn(form, res);
            }
          });
      };

      form.resetAll = function(force){
	if(force || confirm(page.constants.message('navigation.confirmation'))){
	  page.$route.reload();
	  return true;
	}
	return false;
      };

      form.ready = function () {
        return form.$dirty && form.$valid && form.data.auth && (!form.data.password || (!form.data.password.once || form.data.password.once == form.data.password.again));
      };

      //

      form.validator.client({
        email: {
          tips: page.constants.message('party.edit.email.help'),
          errors: page.constants.message('login.email.error'),
        },
        password: {
          tips: page.constants.message('party.edit.password.help'),
        },
        'password.again': {
          tips: page.constants.message('party.edit.password.again.help'),
          errors: page.constants.message('party.edit.password.again.error'),
        },
        auth: {
          tips: page.constants.message('party.edit.auth.help'),
        },
      }, true);

      form.clearPasswordFields = function () {
        form.data.auth = undefined;
        if (form.data.password) {
          form.data.password.again = undefined;
          form.data.password.once = undefined;
        }
      };

      var $float = $('.peac-float');
      var $floater = $('.peac-float-floater');
      $scope.scrollFn = page.display.makeFloatScrollFn($float, $floater, 24*1.5);
      page.$w.scroll($scope.scrollFn);

      //

      page.events.talk('partyEditAccountForm-init', form, $scope);
    };

    //

    return {
      restrict: 'E',
      templateUrl: 'partyEditAccountForm.html',
      scope: false,
      replace: true,
      link: link
    };
  }
]);
