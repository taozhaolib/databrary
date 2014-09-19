'use strict';

module.controller('DataPanel', [
  '$scope', 'pageService', function ($scope, page) {
    $scope.refreshPanel = function () {
      $scope.enabled = $scope.volumes.length || $scope.party.checkPermission(page.permission.EDIT) && page.models.Login.checkAccess(page.permission.PUBLIC);
    };
  }
]);
