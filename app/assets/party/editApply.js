'use strict';

module.directive('partyEditApplyForm', [
  'pageService', function (page) {
    var link = function ($scope) {
      var party = $scope.party;
      var form = $scope.partyEditApplyForm;

      form.data = party.parents.slice();

      $scope.$on('authApplyForm-init', function (event, applyForm) {
        applyForm.successFn = function () {
          form.messages.add({
            body: page.constants.message('auth.apply.save.success'),
            type: 'green',
            countdown: 3000,
          });
        };

        applyForm.cancelFn = function (auth) {
          form.messages.add({
            body: page.constants.message('auth.apply.remove.success'),
            type: 'green',
            countdown: 3000,
          });

          form.data.remove(auth);
        };

        event.stopPropagation();
      });

      var preSelect;
      form.preSelect = function (party) {
	preSelect = party;
      };

      $scope.$on('authSearchForm-init', function (event, searchForm) {
        if (searchForm.principal === 'child')
          return;

        searchForm.selectFn = function (found) {
	  form.data.push({
	    new: true,
	    party: found,
	  });
	  //warning: next line is template dependent! if classnames change this will no longer work
	  page.display.scrollTo('fieldset article.permission-auth.pef:last');
        };

        searchForm.notFoundFn = function (query) {
	  form.data.push({
	    new: true,
	    query: query
	  });
        };

	if (preSelect)
	  searchForm.selectFn(preSelect);
	preSelect = null;
      });

      form.scrollFn = page.display.makeFloatScrollFn($('.peap-float'), $('.peap-float-floater'), 24*2.5);
      page.$w.scroll(form.scrollFn);

      $scope.$emit('partyEditApplyForm-init', form);
    };

    return {
      restrict: 'E',
      templateUrl: 'party/editApply.html',
      link: link
    };
  }
]);
