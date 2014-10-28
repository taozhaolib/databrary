'use strict';

app.directive('fundingSearchForm', [
  'pageService', function (page) {
    var link = function ($scope) {
      var form = $scope.fundingSearchForm;

      form.nameVal = '';
      form.found = [];
      form.sent = false;

      var research;
      form.search = function (all) {
        if (form.nameVal.length < 3)
          return;
        if (form.sent === undefined)
          research = true;
        else {
          form.sent = undefined;
          page.models.funder(form.nameVal, all)
            .then(function (data) {
              form.validator.server({});
              research = false;
              form.sent = !!all;
              form.found = data;
              if (research)
                form.search();
            }, function (res) {
              form.validator.server(res);
            });
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
