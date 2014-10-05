'use strict';

app.controller('volume/browser', [
  '$scope',
  function ($scope) {
    $scope.refreshPanel = function () {
      $scope.enabled = $scope.volume.containers;
    };
  }
]);
