'use strict';

module.directive('accessGrantForm', [
  'pageService', function (page) {
    var link = function ($scope) {
      var volume = $scope.volume;
      var access = $scope.access;
      var form = $scope.accessGrantForm;

      form.data = {
	individual: access.individual,
	extend: access.children === access.individual
      };
      if (access.new)
	form.$setDirty();

      form.canGrantAccess = function (p) {
        return p == page.permission.READ ||
	  p == page.permission.EDIT ||
          p == page.permission.ADMIN;
      };

      form.canChange = 
        form.data.individual !== page.permission.ADMIN || (access.party.id !== page.models.Login.user.id && volume.access && volume.access.filter(function (access) {
          return access.individual === page.permission.ADMIN;
        }).length >= 2);

      //

      form.save = function () {
        form.data.children = form.data.extend ? form.data.individual : 0;

	volume.accessSave(access.party.id, form.data).then(function () {
          form.messages.add({
            body: page.constants.message('access.grant.save.success'),
            type: 'green',
            countdown: 3000,
          });

	  delete access.new;
          form.$setPristine();
        }, function (res) {
          form.messages.addError({
            body: page.constants.message('access.grant.save.error'),
            report: res,
          });

          page.display.scrollTo(form.$element);
        });
      };

      form.remove = function () {
	if (access.new) {
	  form.removeSuccessFn(access);
	  return;
	}
	volume.accessDelete(access.party.id).then(function () {
          form.messages.add({
            body: page.constants.message('access.grant.remove.success'),
            type: 'green',
            countdown: 3000,
          });

          form.$setPristine();
	  form.removeSuccessFn(access);
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
      templateUrl: 'volume/accessGrant.html',
      link: link
    };
  }
]);
