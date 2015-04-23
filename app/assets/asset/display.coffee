'use strict'

app.directive 'assetDisplay', [
  'constantService',
  (constants) ->
    restrict: 'E'
    templateUrl: 'asset/display.html'
    scope:
      assetFn: '&asset'
    link: ($scope) ->
      asset = $scope.assetFn()
      $scope.asset = asset.inContext()
      $scope.readable = $scope.asset.checkPermission(constants.permission.VIEW)

      if $scope.asset != asset
        $scope.clip = asset.segment.relativeTo($scope.asset.segment)
      return
]
