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

      $scope.$on('authGrantForm-init', function (event, grantForm) {
        subforms.push(grantForm);

        grantForm.denySuccessFn = function (auth) {
          form.data.remove(auth);
          subforms.remove(grantForm);
        };

        event.stopPropagation();
      });

      var preSelect;
      form.preSelect = function (party) {
        preSelect = party;
      };

      $scope.$on('authSearchForm-init', function (event, searchForm) {
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

        event.stopPropagation();
      });

      form.scrollFn = page.display.makeFloatScrollFn($('.peg-float'), $('.peg-float-floater'), 24*2.5);
      page.$w.scroll(form.scrollFn);

      $scope.$emit('partyEditGrantForm-init', form);
    };

    return {
      restrict: 'E',
      templateUrl: 'party/editGrant.html',
      link: link
    };
  }
]);
