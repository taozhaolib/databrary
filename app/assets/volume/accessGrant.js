'use strict';

app.directive('accessGrantForm', [
  'pageService', function (page) {
    var link = function ($scope) {
      var volume = $scope.volume;
      var access = $scope.access;
      var form = $scope.accessGrantForm;

      form.data = {
        individual: access.individual,
        extend: access.children === access.individual
      };
      if (!access.individual) {
        form.$setDirty();
        page.$timeout(function () {
          // hack! calling $validate() doesn't work!
          form['access-'+access.party.id+'-individual'].$setValidity('required', false);
        });
      }

      form.canGrantAccess = function (p) {
        return p == page.permission.READ ||
          p == page.permission.EDIT ||
          p == page.permission.ADMIN ||
          page.models.Login.checkAccess(page.permission.ADMIN);
      };

      //

      form.save = function () {
        page.messages.clear(form);
        form.data.children = form.data.extend ? form.data.individual : 0;

        volume.accessSave(access.party.id, form.data).then(function () {
          page.messages.add({
            body: page.constants.message('access.grant.save.success'),
            type: 'green',
            owner: form
          });

          delete access.new;
          form.$setPristine();
        }, function (res) {
          page.messages.addError({
            body: page.constants.message('access.grant.save.error'),
            report: res,
            owner: form
          });

          page.display.scrollTo(form.$element);
        });
      };

      form.remove = function () {
        page.messages.clear(form);
        if (access.new) {
          form.removeSuccessFn(access);
          return;
        }
        volume.accessRemove(access.party.id).then(function () {
          page.messages.add({
            body: page.constants.message('access.grant.remove.success'),
            type: 'green',
            owner: form
          });

          form.$setPristine();
          form.removeSuccessFn(access);
        }, function (res) {
          page.messages.addError({
            body: page.constants.message('access.grant.remove.error'),
            report: res,
            owner: form
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
      templateUrl: 'volume/accessGrant.html',
      link: link
    };
  }
]);
