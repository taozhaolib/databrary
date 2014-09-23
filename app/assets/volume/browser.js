'use strict';

module.controller('volume/browser', [
  '$scope', 'pageService', function ($scope, page) {
    $scope.refreshPanel = function () {
      $scope.enabled = $scope.volume.containers;
    };
  }
]);
