'use strict';

module.directive('accessGrantForm', [
  'pageService', function (page) {
    var link = function ($scope, $element, $attrs) {
      var form = $scope.accessGrantForm;
      form.volume = page.$parse($attrs.volume)($scope) || undefined;
      form.access = page.$parse($attrs.access)($scope) || undefined;

      form.data = {
        individual: form.access.individual || 0,
        children: form.access.children || 0,
      };

      $scope.canGrantAccess = function (p) {
        return  p == page.constants.data.permissionName.READ ||
          p == page.constants.data.permissionName.EDIT ||
          p == page.constants.data.permissionName.ADMIN;
      };

      form.data.extend = form.data.children !== 0;

      var backup = $.extend(true, {}, form.data);

      //

      form.canChange = function () {
        return form.access.individual != 5 || (form.access.party.id != page.auth.user.id && form.volume && form.volume.access && form.volume.access.filter(function (access) {
          return access.individual == 5;
        }).length >= 2);
      };

      //

      form.saveFn = undefined;
      form.successFn = undefined;
      form.errorFn = undefined;

      form.save = function () {
        form.data.children = form.data.extend ? form.data.individual : 0;

        if (angular.isFunction(form.saveFn)) {
          form.saveFn(form);
        }

	form.volume.accessSave(form.access.party.id, form.data).then(function () {
          if (angular.isFunction(form.successFn)) {
            form.successFn(form, arguments);
          }

          form.messages.add({
            body: page.constants.message('access.grant.save.success'),
            type: 'green',
            countdown: 3000,
          });

          backup = $.extend(true, {}, form.data);
          form.$setPristine();
        }, function (res) {
          form.messages.addError({
            body: page.constants.message('access.grant.save.error'),
            report: res,
          });

          if (angular.isFunction(form.errorFn)) {
            form.errorFn(form, arguments);
          }

          page.display.scrollTo(form.$element);
        });
      };

      form.resetFn = undefined;

      form.reset = function () {
        if (angular.isFunction(form.resetFn)) {
          form.resetFn(form);
        }

        form.validator.clearServer();

        form.data = $.extend(true, {}, backup);

        if (form.access.new) {
          form.remove();
        } else {
          form.$setPristine();
        }
      };

      //

      form.removeFn = undefined;
      form.removeSuccessFn = undefined;
      form.removeErrorFn = undefined;

      form.remove = function () {
        if (angular.isFunction(form.removeFn)) {
          form.removeFn(form);
        }

	form.volume.accessDelete(form.access.party.id).then(function () {
          if (angular.isFunction(form.removeSuccessFn)) {
            form.removeSuccessFn(form, arguments, form.access);
          }

          form.messages.add({
            body: page.constants.message('access.grant.remove.success'),
            type: 'green',
            countdown: 3000,
          });

          form.$setPristine();
        }, function (res) {
          form.messages.addError({
            body: page.constants.message('access.grant.remove.error'),
            report: res,
          });

          if (angular.isFunction(form.removeErrorFn)) {
            form.removeErrorFn(form, arguments, form.access);
          }

          page.display.scrollTo(form.$element);
        });
      };

      //

      $scope.$emit('accessGrantForm-init', form, $scope);
    };

    //

    return {
      restrict: 'E',
      templateUrl: 'accessGrantForm.html',
      scope: false,
      replace: true,
      link: link
    };
  }
]);
