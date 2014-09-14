'use strict';

module.directive('authSearchForm', [
  'pageService', function (page) {
    var link = function ($scope, $element, $attrs) {
      var party = $scope.party;
      var form = $scope.authSearchForm;

      form.nameVal = '';
      form.found = [];
      form.principal = $attrs.principal;
      form.apply = form.principal !== 'child';
      form.placeholderText = $attrs.placeholderText || page.constants.message('auth.search.' + (form.principal ? form.principal : 'placeholder'));

      $scope.$watch(function () {
        return form.principal;
      }, function (principal) {
        form.nameVal = '';
        form.found = [];
        form.validator.client({
          name: {
            tips: page.constants.message('auth.search.' + (principal || 'placeholder') + '.help')
          }
        }, true);
      });

      //

      var recentSearch;
      var sentSearch;

      var fin = function () {
        sentSearch = undefined;

        if (recentSearch) {
          recentSearch = undefined;
          form.search();
        }
      };

      form.search = function () {
        if (!form.nameVal || form.nameVal.length < 3) {
          form.found = [];
        } else if (sentSearch) {
          recentSearch = form.nameVal;
        } else {
          sentSearch = party.authorizeSearch(form.apply, {
            name: form.nameVal,
            institution: form.principal === 'principal' ? true :
                form.principal === 'affiliate' ? false : undefined,
          }).then(function (data) {
            form.found = data;

            fin();
          }, fin);
        }
      };

      //

      form.select = function (found) {
        form.nameVal = '';
        form.search();

	form.selectFn(found);
        form.$setPristine();
      };

      //

      form.notFound = function () {
        var query = form.nameVal;

        form.nameVal = '';
        form.search();

	form.notFoundFn(query, form);
        form.$setPristine();
      };

      //

      form.validator.client({
        name: {
          tips: page.constants.message('auth.search.name.help'),
        },
      }, true);

      //

      page.events.talk('authSearchForm-init', form);
    };

    //

    return {
      restrict: 'E',
      templateUrl: 'authSearchForm.html',
      replace: true,
      link: link
    };
  }
]);
