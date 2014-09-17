'use strict';

module.directive('accessSearchForm', [
  'pageService', function (page) {
    var link = function ($scope, $element, $attrs) {
      var volume = $scope.volume;
      var form = $scope.accessSearchForm;

      form.nameVal = '';
      form.found = [];
      form.placeholderText = $attrs.placeholderText || page.constants.messages['access.search'];

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

      form.search = function () {
        if (!form.nameVal || form.nameVal.length < 3) {
          form.found = [];
        } else if (sentSearch) {
          recentSearch = form.nameVal;
        } else {
	  volume.accessSearch({
            name: form.nameVal
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

	$scope.selectFn(found);
        form.$setPristine();
      };

      //

      form.notFound = function () {
        form.messages.add({
          type: 'yellow',
          countdown: 3000,
          body: page.constants.message('access.grant.notfound.message'),
        });

        form.nameVal = '';
        form.search();

        form.$setPristine();
      };

      //

      form.validator.client({
        name: {
          tips: page.constants.message('access.search.name.help'),
        },
      }, true);

    };

    //

    return {
      restrict: 'E',
      templateUrl: 'accessSearchForm.html',
      replace: true,
      link: link
    };
  }
]);
