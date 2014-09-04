'use strict';

module.controller('ExcerptsPanel', [
  '$scope', 'pageService',
  function ($scope, page) {
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
      return asset.format.type === 'image' || asset.format.type === 'video';
    };

    $scope.showThumbnail = function (asset) {
      return asset.format.type === 'image' || asset.asset.duration;
    };

    $scope.jumpLink = function (excerpt) {
      /* maybe should be excerpt.inContext().route or something? */
      return excerpt.container.route;
    };
  }
]);
