'use strict';

module.directive('accessGrantForm', [
  'pageService', function (page) {
    var link = function ($scope, $element, $attrs) {
      var form = $scope.accessGrantForm;
      form.volume = $scope.$eval($attrs.volume);
      form.access = $scope.$eval($attrs.access);

      form.data = {
        individual: form.access.individual || 0,
        children: form.access.children || 0,
      };

      $scope.canGrantAccess = function (p) {
        return  p == page.permission.READ ||
          p == page.permission.EDIT ||
          p == page.permission.ADMIN;
      };

      form.data.extend = form.data.children !== 0;

      var backup = $.extend(true, {}, form.data);

      //

      form.canChange = function () {
        return form.access.individual !== page.permission.ADMIN || (form.access.party.id != page.models.Login.user.id && form.volume && form.volume.access && form.volume.access.filter(function (access) {
          return access.individual === page.permission.ADMIN;
        }).length >= 2);
      };

      //

      form.save = function () {
        form.data.children = form.data.extend ? form.data.individual : 0;

	form.volume.accessSave(form.access.party.id, form.data).then(function () {
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

          page.display.scrollTo(form.$element);
        });
      };

      form.reset = function () {
        form.validator.clearServer();

        form.data = $.extend(true, {}, backup);

        if (form.access.new) {
          form.remove();
        } else {
          form.$setPristine();
        }
      };

      //

      form.removeSuccessFn = undefined;

      form.remove = function () {
	form.volume.accessDelete(form.access.party.id).then(function () {
          if (angular.isFunction(form.removeSuccessFn)) {
            form.removeSuccessFn(form.access);
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
