'use strict';

module.directive('volumeList', [
  'pageService',
  function (page) { return {
    restrict: 'E',
    scope: {
      volumes: "=data",
    },
    templateUrl: 'volume/list.html',
    link: function ($scope) {
      $scope.page = page;

      $scope.name = function (volume) {
        return page.$location.path() === '/profile' && volume.alias || volume.name;
      };
    }
  }; }
]);

