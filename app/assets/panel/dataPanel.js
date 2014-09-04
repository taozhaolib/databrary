'use strict';

module.controller('DataPanel', [
  '$scope', 'pageService', function ($scope, page) {
    $scope.refreshPanel = function () {
      $scope.enabled = $scope.volumes.length || page.models.Login.checkAccess(page.permission.EDIT, $scope.party) && page.models.Login.checkAccess(page.permission.PUBLIC);
    };
  }
]);
