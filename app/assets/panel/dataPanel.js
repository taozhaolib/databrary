'use strict';

module.controller('DataPanel', [
  '$scope', 'pageService', function ($scope, page) {
    $scope.refreshPanel = function () {
      $scope.enabled = $scope.volumes.length || page.auth.hasAccess('EDIT', $scope.party) && page.auth.hasAuth('PUBLIC');
    };
  }
]);
