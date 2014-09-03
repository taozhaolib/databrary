'use strict';

module.controller('ExcerptsPanel', [
  '$scope',
  'pageService',
  function ($scope, page) {
    $scope.bootPanel = function () {
      if (Array.isArray($scope.volume.excerpts) && $scope.volume.excerpts.length > 0) {
        $scope.current = $scope.volume.excerpts[0] || undefined;
      }
    };

    $scope.refreshPanel = function () {
      $scope.enabled = Array.isArray($scope.volume.excerpts) && $scope.volume.excerpts.length > 0;
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

    $scope.listClass = function (excerpt) {
      var cls = [];

      if (excerpt.format.type === 'video') {
        cls.push('video');
      }

      if (excerpt === $scope.current) {
        cls.push('panel-excerpts-list-current');
      }

      return cls;
    };

    $scope.jumpLink = function (excerpt) {
      /* maybe should be excerpt.inContext().route or something? */
      return excerpt.container.route;
    };
  }
]);
