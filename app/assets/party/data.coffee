'use strict'

app.directive 'partyData', [
  'constantService', 'modelService',
  (constants, models) ->
    restrict: 'E'
    templateUrl: 'party/data.html'
    scope: false
    link: ($scope) ->
      $scope.enabled = $scope.volumes.length || $scope.party.checkPermission(constants.permission.EDIT) && models.Login.isAuthorized();
      return
]
