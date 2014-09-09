'use strict';

module.directive('searchForm', [
  'pageService', function (page) {
    var link = function ($scope) {
      var form = $scope.searchForm;
      form.searchFocus = page.storage.get('searchFocus');

      form.data = {
        query: page.$location.search().query
      };

      form.focus = function () {
        if (page.$location.path() !== '/search') {
          page.$location.path('/search');
          page.storage.set('searchFocus', '1');
        }
      };

      form.blur = function () {
        if (page.$location.path() === '/search') {
          page.storage.set('searchFocus', '');
        }
      };

      $scope.$watch('searchForm.data.query', function () {
        page.$location.search('query', form.data.query || undefined);
      });

      //

      page.events.talk('searchForm-init', form, $scope);
    };

    //

    return {
      restrict: 'E',
      templateUrl: 'searchForm.html',
      scope: false,
      replace: true,
      link: link
    };
  }
]);
