'use strict';

module.directive('fundingSearchForm', [
  'pageService', function (page) {
    var link = function ($scope, $element, $attrs) {
      var form = $scope.fundingSearchForm;

      form.nameVal = '';
      form.found = [];
      form.volume = page.$parse($attrs.volume)($scope) || undefined;

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
        var data = {
          query: form.nameVal,
        };

        if (all) {
          data.all = 'true';
        }

        if (!all && (!form.nameVal || form.nameVal.length < 3)) {
          form.found = [];
        } else if (sentSearch) {
          recentSearch = form.nameVal;
        } else {
          sentSearch = page.models.volumeAccess.searchFunding(data,
            function (data) {
              form.found = data;

              fin();
            }, function (res) {
              fin(res);
            });
        }
      };

      //

      form.selectFn = undefined;

      form.select = function (found) {
        form.nameVal = '';
        form.search();

        if (angular.isFunction(form.selectFn)) {
          form.selectFn(found, form);
        }

        form.$setPristine();
      };

      //

      form.notFoundFn = undefined;

      form.notFound = function (found) {
        if (angular.isFunction(form.notFoundFn)) {
          form.notFoundFn(found, form);
        }

        form.search(true);

        form.$setPristine();
      };

      //

      form.validator.client({
        name: {
          tips: page.constants.message('funding.search.name.help'),
        },
      }, true);

      //

      $scope.$emit('fundingSearchForm-init', form);
    };

    //

    return {
      restrict: 'E',
      templateUrl: 'fundingSearchForm.html',
      scope: false,
      replace: true,
      link: link
    };
  }
]);
