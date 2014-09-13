'use strict';

module.directive('authGrantForm', [
  'pageService', function (page) {
    var link = function ($scope, $element, $attrs) {
      var form = $scope.authGrantForm;

      form.party = $scope.party;
      form.other = $scope.$eval($attrs.other);
      if(!form.other.expires){
	var d = new Date();
	d.setFullYear(d.getFullYear()+2);
	form.other.expires = d.getTime();
      }

      form.other.member = form.other.member || 0;
      form.other.site = form.other.site || 0;

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

      form.successFn = undefined;

      form.save = function () {
        if (!form.other.expires) {
          delete form.other.expires;
        } else {
          form.other.expires = page.$filter('date')(form.other.expires, 'yyyy-MM-dd');
        }

	form.party.authorizeSave(form.other.party.id, form.other).then(function () {
          form.validator.server({});
          form.$setPristine();

          if (angular.isFunction(form.successFn)) {
            form.successFn();
          }
        }, function (res) {
          form.validator.server(res);
          page.display.scrollTo(form.$element);
        });
      };

      //

      form.denySuccessFn = undefined;

      form.deny = function () {
        if (form.other.new) {
          if (angular.isFunction(form.denySuccessFn)) {
            form.denySuccessFn();
          }
        } else {
	  form.party.authorizeDelete(form.other.party.id).then(function () {
            form.validator.server({});

            if (angular.isFunction(form.denySuccessFn)) {
              form.denySuccessFn();
            }
          }, function (res) {
            form.validator.server(res);
            page.display.scrollTo(form.$element);
          });
        }
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
      scope: false,
      replace: true,
      link: link,
    };
  }
]);

