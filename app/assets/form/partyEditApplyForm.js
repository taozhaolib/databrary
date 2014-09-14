'use strict';

module.directive('partyEditApplyForm', [
  'pageService', function (page) {
    var link = function ($scope) {
      var party = $scope.party;
      var form = $scope.partyEditApplyForm;

      form.data = party.parents.slice();

      var subforms = [];

      function checkDirty() {
	if (!subforms.some(function (subform) {
	  return subform.$dirty;
	}))
	  form.$setPristine();
      }

      page.events.listen($scope, 'authApplyForm-init', function (event, applyForm) {
        subforms.push(applyForm);

        applyForm.successFn = function () {
          form.messages.add({
            body: page.constants.message('auth.apply.save.success'),
            type: 'green',
            countdown: 3000,
          });
	  checkDirty();
        };

        applyForm.cancelFn = function (auth) {
          form.messages.add({
            body: page.constants.message('auth.apply.remove.success'),
            type: 'green',
            countdown: 3000,
          });

          form.data.remove(auth);
          subforms.remove(applyForm);
	  checkDirty();
        };
      });

      var preSelect;
      form.preSelect = function (party) {
	preSelect = party;
      };

      page.events.listen($scope, 'authSearchForm-init', function (event, searchForm) {
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

      var $float = $('.peap-float');
      var $floater = $('.peap-float-floater');
      form.scrollFn = page.display.makeFloatScrollFn($float, $floater, 24*2.5);
      page.$w.scroll(form.scrollFn);

      page.events.talk('partyEditApplyForm-init', form);
    };

    return {
      restrict: 'E',
      templateUrl: 'partyEditApplyForm.html',
      link: link
    };
  }
]);
