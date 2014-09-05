'use strict';

module.directive('browserPlayer', [
  function () {
    var link = function ($scope) {
      $scope.getAssetClasses = function (asset) {
        return {
          'active': $scope.currentAsset == asset
        };
      };

      $scope.getMimeGroup = function (asset) {
        var mimetype = asset.format.mimetype,
          type = mimetype.split('/')[0];

        return type == 'text' ? mimetype[1] : type;
      };
    };

    return {
      restrict: 'E',
      scope: true,
      templateUrl: 'browserPlayer.html',
      replace: true,
      priority: 100,
      link: link
    };
  }
]);
