'use strict';

app.directive('volumeList', [
  'pageService',
  function (page) { return {
    restrict: 'E',
    scope: {
      volumes: "=data",
    },
    templateUrl: 'volume/list.html',
    link: function ($scope) {
      $scope.page = page;
      $scope.profile = page.$location.path() === '/profile';
      $scope.shared = function (volume) {
        return volume.access.some(function (a) {
          return a.children && a.party.id <= 0; 
        });
      };
      $scope.name = function (volume) {
        console.log(volume);
        return page.$location.path() === '/profile' && volume.alias || volume.name;
      };
    }
  }; }
]);

