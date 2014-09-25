'use strict';

module.directive('authApplyForm', [
  'pageService', function (page) {
    var link = function ($scope) {
      var party = $scope.party || page.models.Login.user;
      var auth = $scope.auth;
      var form = $scope.authApplyForm;

      form.data = {};

      if (auth.new)
	form.$setDirty();

      //

      var saveAuth = function () {
	party.authorizeApply(auth.party.id, form.data).then(function () {
          form.validator.server({});
          form.$setPristine();
          delete auth.new;

	  form.successFn();
        }, function (res) {
          form.validator.server(res);
          page.display.scrollTo(form.$element);
        });
      };

      var saveQuery = function () {
	party.authorizeSearch(true, angular.extend({
          notfound: true,
          name: auth.query
	}, form.data)).then(function () {
          form.validator.server({});
          form.$setPristine();
          delete auth.new;

          form.messages.add({
            type: 'green',
            countdown: 3000,
            body: page.constants.message('auth.request.notfound.success')
          });

	  form.successFn();
        }, function (res) {
          form.validator.server(res);
          page.display.scrollTo(form.$element);
        });
      };

      form.save = function () {
        if (auth.party)
          saveAuth();
	else
          saveQuery();
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

      $scope.$emit('authApplyForm-init', form);
    };

    //

    return {
      restrict: 'E',
      templateUrl: 'party/authApply.html',
      scope: false,
      replace: true,
      link: link
    };
  }
]);
