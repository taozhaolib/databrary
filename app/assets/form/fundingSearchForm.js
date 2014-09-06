'use strict';

module.directive('fundingSearchForm', [
  'pageService', function (page) {
    var link = function ($scope, $element, $attrs) {
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

      form.selectFn = undefined;

      form.select = function (found) {
        form.nameVal = '';
        form.search();

        if (angular.isFunction(form.selectFn)) {
          form.selectFn(found);
        }

        form.$setPristine();
      };

      //

      form.notFound = function (found) {
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
