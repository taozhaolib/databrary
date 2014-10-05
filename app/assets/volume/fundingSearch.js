'use strict';

app.directive('fundingSearchForm', [
  'pageService', function (page) {
    var link = function ($scope) {
      var form = $scope.fundingSearchForm;

      form.nameVal = '';
      form.found = [];

      //

      var recentSearch;
      var sentSearch;

      var fin = function (res) {
        form.validator.server(res || {});

        sentSearch = undefined;

        if (recentSearch) {
          recentSearch = undefined;
          form.search();
        }
      };

      form.search = function (all) {
        $scope.all = all;

        if (!all && (!form.nameVal || form.nameVal.length < 3)) {
          form.found = [];
        } else if (sentSearch) {
          recentSearch = form.nameVal;
        } else {
          sentSearch = page.models.funder(form.nameVal, all)
            .then(function (data) {
              form.found = data;
              fin();
            }, fin);
        }
      };

      //

      form.select = function (found) {
        form.nameVal = '';
        form.search();

        $scope.selectFn(found);
        form.$setPristine();
      };

      //

      form.notFound = function () {
        form.search(true);

        form.$setPristine();
      };

      //

      form.validator.client({
        name: {
          tips: page.constants.message('funding.search.name.help'),
        },
      }, true);

    };

    //

    return {
      restrict: 'E',
      templateUrl: 'volume/fundingSearch.html',
      replace: true,
      link: link
    };
  }
]);
