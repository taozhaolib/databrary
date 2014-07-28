'use strict';

module.directive('authApplyForm', [
  'pageService', function (page) {
    var link = function ($scope, $element, $attrs) {
      var form = $scope.authApplyForm;

      $scope.page = page;

      form.party = page.$parse($attrs.party)($scope) || $scope.party;
      form.other = page.$parse($attrs.other)($scope) || undefined;

      form.notFound = {
	query: form.other && form.other.query || undefined,
	info: undefined
      };

      form.$setDirty();

      //

      form.saveFn = undefined;
      form.successFn = undefined;
      form.errorFn = undefined;

      var saveAuth = function () {
	form.partyAuthorize = new page.models.partyAuthorize();

	if (form.notFound.info) {
	  form.partyAuthorize.info = form.notFound.info;
	}

	form.partyAuthorize.$apply({
	  id: form.party.id,
	  partyId: form.other.party.id
	}, function () {
	  form.validator.server({});
	  page.models.party.$cache.removeAll();
	  form.$setPristine();
	  delete form.other.new;

	  if (angular.isFunction(form.successFn)) {
	    form.successFn(form, arguments);
	  }
	}, function (res) {
	  form.validator.server(res);
	  page.display.scrollTo(form.$element);

	  if (angular.isFunction(form.errorFn)) {
	    form.errorFn(form, arguments);
	  }
	});
      };

      var saveQuery = function () {
	form.partyAuthorize = new page.models.partyAuthorize();

	form.partyAuthorize.$search({
	  id: form.party.id,
	  apply: true,
	  notfound: true,
	  name: form.notFound.query,
	  info: form.notFound.info
	}, function () {
	  form.validator.server({});
	  page.models.party.$cache.removeAll();
	  form.$setPristine();
	  delete form.other.new;

	  form.messages.add({
	    type: 'green',
	    countdown: 3000,
	    body: page.constants.message('auth.request.notfound.success')
	  });

	  if (angular.isFunction(form.successFn)) {
	    form.successFn(form, arguments);
	  }
	}, function (res) {
	  form.validator.server(res);
	  page.display.scrollTo(form.$element);

	  if (angular.isFunction(form.errorFn)) {
	    form.errorFn(form, arguments);
	  }
	});
      };

      form.save = function () {
	if (angular.isFunction(form.saveFn)) {
	  form.saveFn(form);
	}

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
	  form.cancelFn(form);
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
