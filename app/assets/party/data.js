'use strict';

module.controller('party/data', [
  '$scope', 'pageService', function ($scope, page) {
    $scope.refreshPanel = function () {
      $scope.enabled = $scope.volumes.length || $scope.party.checkPermission(page.permission.EDIT) && page.models.Login.isAuthorized();
    };
  }
]);
