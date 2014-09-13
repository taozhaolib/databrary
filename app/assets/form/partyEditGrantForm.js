'use strict';

module.directive('partyEditGrantForm', [
  'pageService', function (page) {
    var link = function ($scope) {
      var party = $scope.party;
      var form = $scope.partyEditGrantForm;

      form.data = [];

      function init() {
	form.data = party.children.map(function (auth) {
	  return {
	    party: auth.party,
	    member: auth.member,
	    site: auth.site,
	    expires: auth.expires
	  };
	});
      }
      init();

      var subforms = [];

      function checkDirty() {
	if (!subforms.some(function (subform) {
	  return subform.$dirty;
	}))
	  form.$setPristine();
      }

      form.saveAll = function () {
        subforms.forEach(function (subform) {
          if (subform.$dirty)
            subform.save(false);
        });
      };

      page.events.listen($scope, 'authGrantForm-init', function (event, grantForm) {
        subforms.push(grantForm);

        grantForm.successFn = function () {
          form.messages.add({
            body: page.constants.message('auth.grant.save.success'),
            type: 'green',
            countdown: 3000,
          });
	  checkDirty();
        };

        grantForm.denySuccessFn = function () {
          form.messages.add({
            body: page.constants.message('auth.grant.remove.success'),
            type: 'green',
            countdown: 3000,
          });

          form.data.splice(form.data.indexOf(grantForm.other), 1);
          subforms.splice(subforms.indexOf(grantForm), 1);
	  checkDirty();
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
	  form.data.push({
	    new: true,
	    party: found,
	    site: 0,
	    member: 0,
	  });
	  //warning: next line is template dependent! if classnames change this will no longer work
	  page.$timeout(function() {
	    var newEl = $('fieldset article.permission-auth.peg').last();
	    page.display.scrollTo(newEl);
	  });
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
