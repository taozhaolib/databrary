'use strict';

module.directive('authGrantForm', [
  'pageService', function (page) {
    var link = function ($scope) {
      var party = $scope.party;
      var auth = $scope.auth;
      var form = $scope.authGrantForm;

      form.data = {
	site: auth.site,
	member: auth.member,
	expires: auth.expires,
      };
      if (auth.new)
	form.$setDirty();

      //

      form.presetName = function (type, name, party) {
        return '<strong>' + page.constants.message('auth.' + type + '.' + name + '.title') + '</strong>: ' + page.$filter('possessive')('auth.' + type + '.' + name, party);
      };

      $scope.canGrantSite = function (p) {
        return  p <= page.constants.permissionName.PUBLIC ||
          p == page.constants.permissionName.READ ||
          p > page.constants.permissionName.READ &&
          page.models.Login.checkAccess(p + 1);
      };

      $scope.canGrantMember = function (p) {
        return  p == page.constants.permissionName.NONE ||
          p == page.constants.permissionName.READ ||
          p == page.constants.permissionName.EDIT ||
          p == page.constants.permissionName.ADMIN;
      };

      //

      form.save = function () {
        if (!form.data.expires)
          delete form.data.expires;
        else
          form.data.expires = page.$filter('date')(form.data.expires, 'yyyy-MM-dd');

	party.authorizeSave(auth.party.id, form.data).then(function () {
          form.validator.server({});
          form.messages.add({
            body: page.constants.message('auth.grant.save.success'),
            type: 'green',
            countdown: 3000,
          });

	  delete auth.new;
          form.$setPristine();
        }, function (res) {
          form.validator.server(res);
          page.display.scrollTo(form.$element);
        });
      };

      //

      form.deny = function () {
        if (auth.new) {
	  form.denySuccessFn(auth);
	  return;
	}
	party.authorizeDelete(auth.party.id).then(function () {
	  form.validator.server({});
	  form.messages.add({
	    body: page.constants.message('auth.grant.remove.success'),
	    type: 'green',
	    countdown: 3000,
	  });
	  form.$setPristine();
	  form.denySuccessFn(auth);
	}, function (res) {
	  form.validator.server(res);
	  page.display.scrollTo(form.$element);
	});
      };

      //

      form.validator.client({
        expires: {
          tips: page.constants.message('auth.grant.expires.help'),
          errors: page.constants.message('auth.grant.expires.error'),
        }
      }, true);

      //

      page.events.talk('authGrantForm-init', form, $scope);
    };

    //

    return {
      restrict: 'E',
      templateUrl: 'authGrantForm.html',
      link: link,
    };
  }
]);

