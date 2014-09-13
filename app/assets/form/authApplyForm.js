'use strict';

module.directive('authApplyForm', [
  'pageService', function (page) {
    var link = function ($scope, $element, $attrs) {
      var form = $scope.authApplyForm;

      $scope.page = page;

      form.party = $scope.party;
      form.other = $scope.$eval($attrs.other);

      form.notFound = {
        query: form.other && form.other.query || undefined,
        info: undefined
      };

      form.$setDirty();

      //

      form.successFn = undefined;

      var saveAuth = function () {
	form.party.authorizeApply(form.other.party.id, {info:form.notFound.info}).then(function () {
          form.validator.server({});
          form.$setPristine();
          delete form.other.new;

          if (angular.isFunction(form.successFn)) {
            form.successFn();
          }
        }, function (res) {
          form.validator.server(res);
          page.display.scrollTo(form.$element);
        });
      };

      var saveQuery = function () {
	form.party.authorizeSearch(true, {
          notfound: true,
          name: form.notFound.query,
          info: form.notFound.info
        }).then(function () {
          form.validator.server({});
          form.$setPristine();
          delete form.other.new;

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

      form.cancelFn = undefined;

      form.cancel = function () {
        if (angular.isFunction(form.cancelFn)) {
          form.cancelFn();
        }
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
