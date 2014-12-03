'use strict';

app.controller('volume/excerpts', [
  '$scope', 'constantService',
  function ($scope, constants) {
    $scope.refreshPanel = function () {
      $scope.enabled = $scope.volume.excerpts && $scope.volume.excerpts.length > 0;
      if ($scope.enabled && !$scope.current)
        $scope.current = $scope.volume.excerpts[0];
    };

    //

    $scope.setCurrent = function (asset) {
      $scope.current = asset;
    };

    $scope.hasThumbnail = function (asset) {
      return asset.checkPermission(constants.permission.READ) && (asset.format.type === 'image' || asset.format.type === 'video' && asset.asset.duration);
    };

    $scope.jumpLink = function (excerpt) {
      return excerpt.asSlot().inContext().route({select:excerpt.segment.format()});
    };
  }
]);
