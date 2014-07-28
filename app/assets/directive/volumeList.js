'use strict';

module.directive('volumeList', [
  'pageService', function (page) {
    var link = function ($scope) {
      $scope.page = page;
      $scope.volumes = [];

      $scope.$watch('query', function () {
        $scope.volumes = page.$filter('filter')($scope.data, $scope.query);
      });

      //

      $scope.volumeClasses = function (volume) {
        var cls = [];

        if (volume.citation) {
          cls.push('study');
        } else {
          cls.push('dataset');
        }

        return cls;
      };

      $scope.iconClass = function (volume) {
        var cls = [];

        if (volume.citation) {
          cls.push('study');
        } else {
          cls.push('dataset');
        }

        return cls;
      };

      $scope.name = function (volume) {
        return volume.alias && page.$location.path() === '/profile' ? volume.alias : volume.name;
      };
    };

    return {
      restrict: 'E',
      replace: false,
      scope: {
        data: "=",
        query: "=?"
      },
      templateUrl: 'volumeList.html',
      link: link,
    };
  }
]);

