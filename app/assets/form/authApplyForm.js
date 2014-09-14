'use strict';

module.directive('authApplyForm', [
  'pageService', function (page) {
    var link = function ($scope) {
      var party = $scope.party;
      var auth = $scope.auth;
      var form = $scope.authApplyForm;

      $scope.page = page;

      form.notFound = {
        query: auth.query,
        info: undefined
      };

      form.$setDirty();

      //

      form.successFn = undefined;

      var saveAuth = function () {
	party.authorizeApply(auth.party.id, {info:form.notFound.info}).then(function () {
          form.validator.server({});
          form.$setPristine();
          delete auth.new;

          if (angular.isFunction(form.successFn)) {
            form.successFn();
          }
        }, function (res) {
          form.validator.server(res);
          page.display.scrollTo(form.$element);
        });
      };

      var saveQuery = function () {
	party.authorizeSearch(true, {
          notfound: true,
          name: form.notFound.query,
          info: form.notFound.info
        }).then(function () {
          form.validator.server({});
          form.$setPristine();
          delete auth.new;

          form.messages.add({
            type: 'green',
            countdown: 3000,
            body: page.constants.message('auth.request.notfound.success')
          });

          if (angular.isFunction(form.successFn)) {
            form.successFn();
          }
        }, function (res) {
          form.validator.server(res);
          page.display.scrollTo(form.$element);
        });
      };

      form.save = function () {
        if (form.notFound.query) {
          saveQuery();
        } else {
          saveAuth();
        }
      };

      //

      form.cancel = function () {
	form.cancelFn(auth);
      };

      //

      form.validator.client({
        info: {
          tips: page.constants.message('auth.request.info.help'),
        },
      }, true);

      //

      page.events.talk('authApplyForm-init', form, $scope);
    };

    //

    return {
      restrict: 'E',
      templateUrl: 'authApplyForm.html',
      scope: false,
      replace: true,
      link: link
    };
  }
]);
