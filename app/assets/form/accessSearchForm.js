'use strict';

module.directive('accessSearchForm', [
  'pageService', function (page) {
    var link = function ($scope, $element, $attrs) {
      var form = $scope.accessSearchForm;

      form.volume = $scope.$eval($attrs.volume);
      form.nameVal = '';
      form.found = [];
      form.institution = $element.attr('institution') === 'true';
      form.placeholderText = $attrs.placeholderText || page.constants.messages['access.search'];

      $attrs.$observe('institution', function () {
        form.nameVal = '';
        form.found = [];
      });

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
	  form.volume.accessSearch({
            name: form.nameVal,
            institution: form.institution,
          }).then(function (data) {
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

      form.notFound = function () {
        form.messages.add({
          type: 'yellow',
          countdown: 3000,
          body: page.constants.message('access.grant.notfound.message'),
        });

        var query = form.nameVal;

        form.nameVal = '';
        form.search();

        if (angular.isFunction(form.notFoundFn)) {
          form.notFoundFn(query, form);
        }

        form.$setPristine();
      };

      //

      form.validator.client({
        name: {
          tips: page.constants.message('access.search.name.help'),
        },
      }, true);

      //

      $scope.$emit('accessSearchForm-init', form);
    };

    //

    return {
      restrict: 'E',
      templateUrl: 'accessSearchForm.html',
      scope: false,
      replace: true,
      link: link
    };
  }
]);
