'use strict';

module.controller('BrowserPanel', [
  '$scope', 'pageService', function ($scope, page) {
    $scope.refreshPanel = function () {
      switch (page.$route.current.controller) {
        case 'volumeView':
          $scope.enabled = $scope.volume.containers;
          break;
        case 'partyView':
          $scope.enabled = $scope.volumes[0];
          break;
      }
    };
  }
]);
