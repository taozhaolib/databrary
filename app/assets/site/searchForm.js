'use strict';

module.directive('searchForm', [
  'pageService',
  function (page) { return {
    restrict: 'E',
    templateUrl: 'site/searchForm.html',
    scope: true,
    link: function ($scope) {
      $scope.search = function () {
        page.$location.path(page.router.search()).search($scope.search.data);
      };
      $scope.search.data = {};
    }
  }; }
]);
