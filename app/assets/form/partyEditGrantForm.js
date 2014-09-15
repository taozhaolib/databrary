'use strict';

module.directive('partyEditGrantForm', [
  'pageService', function (page) {
    var link = function ($scope) {
      var party = $scope.party;
      var form = $scope.partyEditGrantForm;

      form.data = party.children.slice();

      var subforms = [];

      form.saveAll = function () {
        subforms.forEach(function (subform) {
          if (subform.$dirty)
            subform.save(false);
        });
      };

      page.events.listen($scope, 'authGrantForm-init', function (event, grantForm) {
        subforms.push(grantForm);

        grantForm.denySuccessFn = function (auth) {
          form.data.remove(auth);
          subforms.remove(grantForm);
        };
      });

      var preSelect;
      form.preSelect = function (party) {
	preSelect = party;
      };

      page.events.listen($scope, 'authSearchForm-init', function (event, searchForm) {
        if (searchForm.principal !== 'child')
          return;

        searchForm.selectFn = function (found) {
	  var exp = new Date();
	  exp.setFullYear(exp.getFullYear()+2);
	  form.data.push({
	    new: true,
	    party: found,
	    site: 0,
	    member: 0,
	    expires: exp.getTime()
	  });
	  //warning: next line is template dependent! if classnames change this will no longer work
	  page.display.scrollTo('fieldset article.permission-auth.peg:last');
        };

        searchForm.notFoundFn = function () {
          page.messages.add({
            type: 'yellow',
            countdown: 3000,
            body: page.constants.message('auth.grant.notfound')
          });
        };

	if (preSelect)
	  searchForm.selectFn(preSelect);
	preSelect = null;
      });

      var $float = $('.peg-float');
      var $floater = $('.peg-float-floater');
      form.scrollFn = page.display.makeFloatScrollFn($float, $floater, 24*2.5);
      page.$w.scroll(form.scrollFn);

      page.events.talk('partyEditGrantForm-init', form, $scope);
    };

    return {
      restrict: 'E',
      templateUrl: 'partyEditGrantForm.html',
      link: link
    };
  }
]);
