'use strict';

module.controller('DataPanel', [
  '$scope', 'pageService', function ($scope, page) {
    $scope.refreshPanel = function () {
      $scope.enabled = $scope.volumes.length || page.auth.hasAccess(page.permission.EDIT, $scope.party) && page.auth.hasAuth(page.permission.PUBLIC);
    };
  }
]);
